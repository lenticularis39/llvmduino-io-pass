#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

namespace {
    struct SimplifyIOPass : public FunctionPass {
        static char ID;
        SimplifyIOPass() : FunctionPass(ID) {}

        /// Replaces the instance of digitalWrite with the actual writing to the
        /// appropriate pin.
        void replaceDigitalWrite(CallInst *CI) {
            Value *pinNumberValue = CI->getOperand(0);
            Value *outputStateValue = CI->getOperand(1);
            errs() << *outputStateValue << "\n";
            if (!isa<ConstantInt>(pinNumberValue)) {
                // Pin number is not constant - cannot replace
                return;
            }
            int pinNumber = dyn_cast<ConstantInt>(pinNumberValue)->getZExtValue();
            bool outputState = dyn_cast<ConstantInt>(outputStateValue)->getZExtValue();
            int portAddress, portMask;

            if (pinNumber <= 7) {
                portAddress = 0x2B; // PORTD
                portMask = 1 << (pinNumber);

            }
            else if (pinNumber <= 13) {
                portAddress = 0x25; // PORTB
                portMask = 1 << (pinNumber - 7 - 1);
            }
            else {
                portAddress = 0x28; // PORTC
                portMask = 1 << (pinNumber - 13 - 1);
            }

            IntToPtrInst *portAddressIntToPtr =
                new IntToPtrInst(ConstantInt::get(IntegerType::get(CI->getContext(), 8),
                                                  portAddress),
                                 PointerType::get(IntegerType::get(CI->getContext(), 8), 0),
                                 "",
                                 CI);
            LoadInst *originalPortValue = new LoadInst(portAddressIntToPtr, "", CI);
            BinaryOperator *binOperator;

            if(outputState) {
                // HIGH
                binOperator = BinaryOperator::Create(Instruction::BinaryOps::Or, originalPortValue,
                                        ConstantInt::get(IntegerType::get(CI->getContext(), 8), portMask),
                                        "", CI);
            } else {
                // LOW
                binOperator = BinaryOperator::Create(Instruction::BinaryOps::And, originalPortValue,
                                        ConstantInt::get(IntegerType::get(CI->getContext(), 8), ~portMask),
                                        "", CI);
            }

            StoreInst *writeValue = new StoreInst(binOperator, portAddressIntToPtr, "", CI);
        }

        virtual bool runOnFunction(Function &F) {
            if(F.getName() != "setup" && F.getName() != "loop")
                return false;

            std::vector<Instruction *> instructionsToDelete;

            for (BasicBlock &BB : F) {
                for (Instruction &I : BB) {
                    if (CallInst *CI = dyn_cast<CallInst>(&I)) {
                        if (CI->getCalledFunction()->getName() == "digitalWrite") {
                            replaceDigitalWrite(CI);
                            instructionsToDelete.push_back(CI);
                        }
                    }
                }
            }

            for (Instruction *I : instructionsToDelete) {
                I->eraseFromParent();
            }

            return true;
        }
    };
}

char SimplifyIOPass::ID = 0;

static RegisterPass<SimplifyIOPass> X("siop", "Simplify IO pass",
                             true /* Only looks at CFG */,
                             true);

static void registerSimplifyIOPass(const PassManagerBuilder &,
                                   legacy::PassManagerBase &PM) {
    PM.add(new SimplifyIOPass());
}

static RegisterStandardPasses
RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
                registerSimplifyIOPass);
