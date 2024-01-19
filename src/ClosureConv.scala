package compiler
import NewCPS._

object ClosureConv {

    type EnvType = Seq[KVal | FnPointer]
    case class FnPointer(s: String)
    case class Env(name: String, vals: EnvType)
    case class Ref(name: String, idx: Int)
    
    case class KLetEnv(x: String, env: Env, next: KAnf) extends KAnf
    case class KLetEnvRef(x: String, ref: Ref, next: KAnf) extends KAnf
    
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

    def convert(e: KAnf, glob_fns: List[String] = Nil): KAnf = e match {
        case KFun(fnName, args, body, in) => {
            val env = Fresh("env")
            val fvs = (free_anf(body) -- args).toList
            println(s"The free variables in $fnName are $fvs")

            def convertLocalToEnvRef(next: KAnf, i: Int)(ssaVar: String): (KAnf, Int) = {
                (KLetEnvRef(ssaVar, Ref(env, i), next), i + 1)
            }

            val (body2, _) = if fvs.isEmpty
            then (convert(body, fnName :: glob_fns), 1)
            else fvs.zipWithIndex.foldLeft((convert(body, glob_fns), 1)) {
                // LET x = env[i] IN anf
                case ((anf, i), x) => convertLocalToEnvRef(anf, i)(x._1)
            }

            if fvs.isEmpty then (KFun(fnName, args, body2, convert(in, fnName :: glob_fns)))
            else {
                val vs = fvs.map(KVar)
                // LET fn = (@fn, fvs...) IN anf
                val in2 = KLetEnv(fnName, Env(env, FnPointer(fnName) :: vs), convert(in, glob_fns))
                (KFun(fnName, env +: args, body2, in2))
            }

        }
        case KLet(x, KCall(fn, vrs), e2) => {
            println(s"Checking if $fn is in $glob_fns")
            val ptr = Fresh("ptr")
            if glob_fns.contains(fn) then KLet(x, KCall(fn, vrs), convert(e2, glob_fns))
            else KLetEnvRef(ptr, Ref(fn, 0), KLet(x, KCall(ptr, KVar(fn) +: vrs), convert(e2, glob_fns)))
        }
        case KLet(x, e1, e2) => KLet(x, e1, convert(e2, glob_fns))
        case KIf(v, e1, e2) => KIf(v, convert(e1), convert(e2, glob_fns))
        case _ => e
    }

    def hoist(e: KAnf): (List[CFunc], KAnf, List[Env]) = e match {
        case KFun(fnName, args, body, next) => {
            val (fns, e, envs) = hoist(body)
            val (fns2, e2, envs2) = hoist(next)
            val entry = Fresh("entry")
            val fn = CFunc(fnName, args, e)
            (fn :: fns ::: fns2, e2, envs ::: envs2)
        }
        case KIf(x1, e1, e2) => {
            val (fns, t, envs) = hoist(e1)
            val (fns2, f, envs2) = hoist(e2)
            val thn = Fresh("then")
            val els = Fresh("else")
            (fns ::: fns2, KIf(x1, t, f), envs ::: envs2)
        }
        case KLetEnv(x, env: Env, next) => {
            val (fns, e1, envs) = hoist(next)
            (fns, KLetEnv(x, env, e1), env :: envs)
        }
        case KLet(x, v, next) => {
            val (fns, e1, envs) = hoist(next)
            (fns, KLet(x, v, e1), envs)
        }
        case _ => (Nil, e, Nil)
    }

    def remove_expval(a: KAnf): KAnf = a match {
        case KLet(x, e1, KLet(y, KExpVal(_), e2)) => KLet(y, e1, remove_expval(e2))
        case KIf(x, e1, e2) => KIf(x, remove_expval(e1), remove_expval(e2))
        case KLetEnv(x, env, e2) => KLetEnv(x, env, remove_expval(e2))
        case KLetEnvRef(x, ref, e2) => KLetEnvRef(x, ref, remove_expval(e2))
        case KLet(x, v, e2) => KLet(x, v, remove_expval(e2))
        case KFun(fnName, args, body, in) => KFun(fnName, args, remove_expval(body), remove_expval(in))
        case KReturn(v) => KReturn(v)
    }
}
