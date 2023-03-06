import scala.language.implicitConversions

import isl._
import java.math.BigInteger

package isl {
  object Conversions {
    implicit def convertLambdaToVoidCallback1[ArgTy1](f: ArgTy1 => _): VoidCallback1[ArgTy1] = {
      new VoidCallback1[ArgTy1]() {
        def apply(arg1: ArgTy1) = f(arg1)
      }
    }

    implicit def convertLambdaToVoidCallback2[ArgTy1,ArgTy2](f: (ArgTy1,ArgTy2) => _): VoidCallback2[ArgTy1,ArgTy2] = {
      new VoidCallback2[ArgTy1,ArgTy2]() {
        def apply(arg1: ArgTy1, arg2: ArgTy2) = f(arg1,arg2)
      }
    }

    implicit def convertLambdaToVoidCallback3[ArgTy1,ArgTy2,ArgTy3](f: (ArgTy1,ArgTy2,ArgTy3) => _): VoidCallback3[ArgTy1,ArgTy2,ArgTy3] = {
      new VoidCallback3[ArgTy1,ArgTy2,ArgTy3]() {
        def apply(arg1: ArgTy1, arg2: ArgTy2, arg3: ArgTy3) = f(arg1,arg2,arg3)
      }
    }

    implicit def convertLambdaToCallback1[ArgTy1,RetTy](f: ArgTy1 => RetTy): Callback1[ArgTy1,RetTy] = {
      new Callback1[ArgTy1,RetTy]() {
        def apply(arg1: ArgTy1) = f(arg1)
      }
    }

    implicit def convertLambdaToCallback2[ArgTy1,ArgTy2,RetTy](f: (ArgTy1,ArgTy2) => RetTy): Callback2[ArgTy1,ArgTy2,RetTy] = {
      new Callback2[ArgTy1,ArgTy2,RetTy]() {
        def apply(arg1: ArgTy1, arg2: ArgTy2) = f(arg1,arg2)
      }
    }

    implicit def convertLambdaToCallback3[ArgTy1,ArgTy2,ArgTy3,RetTy](f: (ArgTy1,ArgTy2,ArgTy3) => RetTy): Callback3[ArgTy1,ArgTy2,ArgTy3,RetTy] = {
      new Callback3[ArgTy1,ArgTy2,ArgTy3,RetTy]() {
        def apply(arg1: ArgTy1, arg2: ArgTy2, arg3: ArgTy3) = f(arg1,arg2,arg3)
      }
    }

    implicit def convertValToBigInt(v: Val): BigInt = {
      assert(v.getDen() == BigInteger.ONE)
      new BigInt(v.getNum())
    }

//  In the current bindings, isl.Val requires an isl.Ctx argument, so we cannot
//  define implicit conversions to isl.Val :-(

//    implicit def convertIntToVal(i: Int): Val = new Val(i.toString())
//    implicit def convertBigIntToVal(i: BigInt): Val = new Val(i.toString())
    implicit def convertBigIntegerToBigInt(i: java.math.BigInteger): BigInt = new BigInt(i)
//    implicit def convertBigIntegerToVal(i: java.math.BigInteger): BigInt = new Val(i.toString())
  }
}
