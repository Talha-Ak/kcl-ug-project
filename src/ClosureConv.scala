package compiler
import NewCPS._

object ClosureConv {

    sealed case class CFunc(fname: String, args: Seq[String], body: KAnf) {
        override def toString = s"fun $fname($args):\n$body\nEND"
    }

    def free_val(v: KVal): Set[String] = v match {
        case KVar(s) => Set(s)
        case KNum(i) => Set()
    }

    def free_exp(e: KExp): Set[String] = e match {
        case Kop(o, v1, v2) => free_val(v1) ++ free_val(v2)
        case KCall(o, vrs) => vrs.flatMap(free_val).toSet
        case KExpVal(v) => free_val(v)
    }


    def free_anf(e: KAnf): Set[String] = e match {
        case KLet(x, e1, e2) => free_exp(e1) ++ free_anf(e2) - x
        case KIf(x, e1, e2) => free_anf(e1) ++ free_anf(e2)
        case KReturn(v) => free_val(v)
        case KFun(fnName, args, body, in) => free_anf(body) ++ free_anf(in) -- Set(fnName) -- args
    }

    def convert(e: KAnf): KAnf = e match {
        case KFun(fnName, args, body, in) => {
            val env = Fresh("env")
            val fvs = (free_anf(body) -- args).toList
            println(s"The free variables in $fnName are $fvs")
            val (body2, _) = fvs.zipWithIndex.foldLeft((convert(body), 1)) {
                // LET x = env[i] IN anf
                case ((anf, i), x) => (KLet(x._1, KEnvRef(env, i), anf), i+1)
            }
            val vs = fvs.map(KVar)
            // LET fn = (@fn, fvs...) IN anf
            val in2 = KLet(fnName, KEnv(KFnPointer(fnName) :: vs), in)
            KFun(fnName, env +: args, body2, in2)
        }
        case KLet(x, KCall(fn, vrs), e2) => {
            val ptr = Fresh("ptr")
            KLet(ptr, KEnvRef(fn, 0), KLet(x, KCall(ptr, KVar(fn) +: vrs), convert(e2)))
        }
        case KLet(x, e1, e2) => KLet(x, e1, convert(e2))
        case KIf(v, e1, e2) => KIf(v, convert(e1), convert(e2))
        case _ => e
    }

    def hoist(e: KAnf): (List[CFunc], KAnf) = e match {
        case KFun(fnName, args, body, in) => {
            val (fns, e) = hoist(body)
            val (fns2, e2) = hoist(in)
            val entry = Fresh("entry")
            val fn = CFunc(fnName, args, e)
            (fn :: fns ::: fns2, e2)
        }
        case KIf(x1, e1, e2) => {
            val (fns, t) = hoist(e1)
            val (fns2, f) = hoist(e2)
            val thn = Fresh("then")
            val els = Fresh("else")
            (fns ::: fns2, KIf(x1, t, f))
        }
        case KLet(x, v, in) => {
            val (fns, e1) = hoist(in)
            (fns, KLet(x, v, e1))
        }
        case _ => (Nil, e)
    }
}
