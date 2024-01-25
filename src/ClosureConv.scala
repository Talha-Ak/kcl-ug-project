package compiler
import NewCPS._
import ValParser.Type
import compiler.ValParser.FnType
import compiler.ValParser.Missing

object ClosureConv {

    case class Env(name: String, vals: Seq[KVal])
    case class Ref(name: String, idx: Int)
    case class EnvType(env: String) extends Type

    case class KLetEnv(x: String, env: Env, next: KAnf) extends KAnf {
        override def toString = s"LET $x = $env \n$next"
    }
    case class KLetEnvRef(x: String, ref: Ref, next: KAnf) extends KAnf {
        override def toString = s"LET $x = $ref \n$next"
    }
    
    sealed case class CFunc(fname: String, args: Seq[(String, Type)], ret: Type, body: KAnf) {
        override def toString = s"fun $fname($args):\n$body\nEND"
    }

    def free_val(v: KVal): Set[KVar] = v match {
        case k: KVar => Set(k)
        case _ => Set()
    }

    def free_exp(e: KExp): Set[KVar] = e match {
        case Kop(o, v1, v2) => free_val(v1) ++ free_val(v2)
        case KCall(o, vrs) => vrs.flatMap(free_val).toSet
        case KExpVal(v) => free_val(v)
    }

    def free_anf(e: KAnf): Set[KVar] = e match {
        case KLet(x, e1, e2) => (free_exp(e1) ++ free_anf(e2)).filterNot(_.s == x)
        case KIf(x, e1, e2) => free_anf(e1) ++ free_anf(e2)
        case KReturn(v) => free_val(v)
        case KFun(fnName, args, _, body, in) => (free_anf(body) ++ free_anf(in)).filterNot(x => args.map(_._1).contains(x.s) || x.s == fnName)
    }
    
    // crime committed here
    var glob_fns: Set[String] = Set()

    def convert(e: KAnf): (KAnf, Option[Env]) = e match {
        case KFun(fnName, args, ret, body, in) => {
            val envId = Fresh("env")
            val fvs = free_anf(body).filterNot(x => args.map(_._1).contains(x.s)).toList
            println(s"The free variables in $fnName are $fvs")

            if fvs.isEmpty then {
                glob_fns += fnName
                val (body2, env) = convert(body)
                val (in2, _) = convert(in)
                env match
                    case None => (KFun(fnName, args, ret, body2, in2), None)
                    case Some(e) => (KFun(fnName, args, EnvType(e.name), body2, in2), None)
            }
            else {
                val env = Env(envId, KVar(fnName, Missing) :: fvs)

                val ((body2, next_env), _) = fvs.zipWithIndex.foldLeft((convert(body), 1)) { // missing nested envs
                    // LET x = env[i] IN anf
                    case ((next, i), (ssaVar, _)) => ((KLetEnvRef(ssaVar.s, Ref(envId, i), next._1), next._2), i + 1)
                }

                // LET fn = (@fn, fvs...) IN anf
                val (in2, _) = convert(in)
                val let_env = KLetEnv(fnName, env, in2)
                next_env match
                    case None => (KFun(fnName, (envId, EnvType(envId)) +: args, ret, body2, let_env), Some(env))
                    case Some(e) => (KFun(fnName, (envId, EnvType(envId)) +: args, EnvType(e.name), body2, let_env), Some(env))
            }
        }
        case KLet(x, KCall(fn, vrs), e2) => {
            val ptr = Fresh("ptr")
            if glob_fns.contains(fn) then
                val (in, env) = convert(e2)
                (KLet(x, KCall(fn, vrs), in), env)
            else
                val (in, env) = convert(e2)
                (KLetEnvRef(ptr, Ref(fn, 0), KLet(x, KCall(ptr, KVar(fn) +: vrs), in)), env)
        }
        case KLet(x, e1, e2) =>
            val (in, env) = convert(e2)
            (KLet(x, e1, in), env)
        case KIf(v, e1, e2) =>
            val (t, env_t) = convert(e1)
            val (f, env_f) = convert(e2)
            (KIf(v, t, f), env_t.orElse(env_f))
        case _ => (e, None)
    }

    def hoist(e: KAnf): (List[CFunc], KAnf, List[Env]) = e match {
        case KFun(fnName, args, ret, body, next) => {
            val (fns, e, envs) = hoist(body)
            val (fns2, e2, envs2) = hoist(next)
            val entry = Fresh("entry")
            val fn = CFunc(fnName, args, ret, e)
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
        case KFun(fnName, args, ret, body, in) => KFun(fnName, args, ret, remove_expval(body), remove_expval(in))
        case KReturn(v) => KReturn(v)
    }
}
