package compiler
import ValParser.{Type, FnType, EnvType}
import compiler.ValParser.Missing
import compiler.ValParser.VoidType
import compiler.ValParser.IntType
import compiler.ValParser.BoolType
import compiler.ValParser.FloatType
import compiler.ValParser.UserType
import compiler.ValParser.EnumType

case class CFunc(fname: String, args: Seq[(String, Type)], ret: Type, body: KAnf) {
    override def toString = s"fun $fname($args):\n$body\nEND"
}

class ClosureConv(counter: Counter) {

    def free_val(v: KVal): Set[KVar] = v match {
        case k: KVar => Set(k)
        case _ => Set()
    }

    def free_exp(e: KExp): Set[KVar] = e match {
        case Kop(o, v1, v2) => free_val(v1) ++ free_val(v2)
        case KStructDec(struct, vals) => vals.flatMap(free_val).toSet
        case KCall(o, vrs) => vrs.flatMap(free_val).toSet
    }

    def free_anf(e: KAnf): Set[KVar] = e match {
        case KLet(x, e1, e2) => (free_exp(e1) ++ free_anf(e2)).filterNot(_.s == x)
        case KConst(x, v, e) => free_val(v) ++ free_anf(e)
        case KIf(x, e1, e2) => free_anf(e1) ++ free_anf(e2)
        case KFun(fnName, args, _, body, in) => (free_anf(body) ++ free_anf(in)).filterNot(x => args.map(_._1).contains(x.s) || x.s == fnName)
        case KStructDef(_, in) => free_anf(in)
        case KStructRef(x, _, _, _, in) => free_anf(in).filterNot(_.s == x)
        case KWrite(v, in) => free_val(v) ++ free_anf(in)
        case KReturn(v) => free_val(v)
    }
    
    // crime committed here
    var glob_fns: Set[String] = Set()

    def convert(e: KAnf): (KAnf, Option[Env]) = e match {
        case KFun(fnName, args, ret, body, in) => {
            val fvs = free_anf(body).filterNot(x => args.map(_._1).contains(x.s)).toList
            println(s"The free variables in $fnName are $fvs")

            if fvs.isEmpty then {
                glob_fns += fnName
                val (body2, env) = convert(body)
                val (in2, _) = convert(in)
                env match
                    case None => (KFun(fnName, args, ret, body2, in2), None)
                    case Some(e) =>
                        val updated_type = FnType(args.map(_._2), EnvType(e.name))
                        val updated_body = update_types(body2, Map(fnName -> updated_type))
                        val updated_in = update_types(in2, Map(fnName -> updated_type))
                        (KFun(fnName, args, EnvType(e.name), updated_body, updated_in), None)
            } else {
                val (converted_body, next_env) = convert(body)

                val envId = counter.Fresh("env")
                val newArgs = (envId, EnvType(envId)) +: args
                val newRet = if next_env.isEmpty then ret else EnvType(next_env.get.name)
                val newType = FnType(newArgs.map(_._2), newRet)
                val env = Env(envId, KVar(fnName, newType) :: fvs)
 
                // For each free variable, add a line that extracts it from the environment
                val (body2, _) = fvs.zipWithIndex.foldLeft(converted_body, 1) {
                    // LET x = env[i] IN anf
                    case ((next, i), (ssaVar, _)) => (KLetEnvRef(ssaVar.s, Ref(KVar(envId, EnvType(envId)), i), next), i + 1)
                }

                val (in2, _) = convert(in)
                // LET fn = (@fn, fvs...) IN anf
                val let_env = KLetEnv(fnName, env, in2)

                val updated_body = update_types(body2, Map(fnName -> newType))
                val updated_in = update_types(let_env, Map(fnName -> newType))
                (KFun(fnName, newArgs, newRet, updated_body, updated_in), Some(env))
            }
        }
        case KLet(x, KCall(fn, vrs), e2) =>
            val (in, env) = convert(e2)
            (KLet(x, KCall(fn, vrs), in), env)
        case KLet(x, e1, e2) =>
            val (in, env) = convert(e2)
            (KLet(x, e1, in), env)
        case KIf(v, e1, e2) =>
            val (t, env_t) = convert(e1)
            val (f, env_f) = convert(e2)
            (KIf(v, t, f), env_t.orElse(env_f))
        case KConst(x, v, e) => 
            val (in, env) = convert(e)
            (KConst(x, v, in), env)
        case KStructDef(struct, e) =>
            val (in, env) = convert(e)
            (KStructDef(struct, in), env)
        case _ => (e, None)
    }

    def hoist(e: KAnf): (List[CFunc], KAnf, List[Env], List[Struct]) = e match {
        case KFun(fnName, args, ret, body, next) => {
            val (fns, e, envs, structs) = hoist(body)
            val (fns2, e2, envs2, structs2) = hoist(next)
            val entry = counter.Fresh("entry")
            val fn = CFunc(fnName, args, ret, e)
            (fn :: fns ::: fns2, e2, envs ::: envs2, structs ::: structs2)
        }
        case KIf(x1, e1, e2) => {
            val (fns, t, envs, structs) = hoist(e1)
            val (fns2, f, envs2, structs2) = hoist(e2)
            val thn = counter.Fresh("then")
            val els = counter.Fresh("else")
            (fns ::: fns2, KIf(x1, t, f), envs ::: envs2, structs ::: structs2)
        }
        case KLetEnv(x, env: Env, next) => {
            val (fns, e1, envs, structs) = hoist(next)
            (fns, KLetEnv(x, env, e1), env :: envs, structs)
        }
        case KLet(x, v, next) => {
            val (fns, e1, envs, structs) = hoist(next)
            (fns, KLet(x, v, e1), envs, structs)
        }
        case KConst(x, v, e) => {
            val (fns, e1, envs, structs) = hoist(e)
            (fns, KConst(x, v, e1), envs, structs)
        }
        case KStructDef(struct, e) => {
            val (fns, e1, envs, structs) = hoist(e)
            (fns, KStructDef(struct, e1), envs, struct :: structs)
        }
        case _ => (Nil, e, Nil, Nil)
    }

    def update_types(a: KAnf, ty: Map[String, Type]): KAnf = a match {
        case KLet(x, e1, e2) => e1 match {

            case Kop(o, v1, v2) =>
                val (uv1, v1_ty) = update_val_types(v1, ty)
                KLet(x, Kop(o, uv1, v2), update_types(e2, ty + (x -> v1.get_type)))

            case KCall(o, vrs) => 
                val call_ty = ty.getOrElse(o.s, o.t)
                call_ty match {
                    case FnType(args, ret) => {
                        val (updated_vals, updated_vals_ty) = vrs.foldLeft((Seq[KVal](), ty)) {
                            case ((vs, t), v) =>
                                val (uv, t2) = update_val_types(v, t)
                                (vs :+ uv, t2)
                        }
                        KLet(x, KCall(KVar(o.s, call_ty), updated_vals), update_types(e2, updated_vals_ty + (x -> ret)))
                    }
                    case EnvType(env) => {
                        val (updated_vals, updated_vals_ty) = vrs.foldLeft((Seq[KVal](), ty)) {
                            case ((vs, t), v) =>
                                val (uv, t2) = update_val_types(v, t)
                                (vs :+ uv, t2)
                        }
                        val ptr = counter.Fresh("ptr")
                        val env_fn = KVar(o.s, call_ty)
                        KLetEnvRef(ptr, Ref(env_fn, 0), KLet(x, KCall(KVar(ptr, o.t), env_fn +: updated_vals), update_types(e2, updated_vals_ty + (x -> EnvType(env)))))
                    }
                    case _ => throw new Exception(s"$ty \nExpected function type for $o, got ${ty(o.s)} $vrs")
                }
        }
        case KFun(fnName, args, ret, body, in) =>
            val fn_ty = ty.getOrElse(fnName, FnType(args.map(_._2), ret))
            val body_ty = ty ++ args + (fnName -> fn_ty)
            val body2 = update_types(body, body_ty)
            val in_ty = ty + (fnName -> fn_ty)
            val in2 = update_types(in, in_ty)
            KFun(fnName, args, ret, body2, in2)
        case KIf(x, e1, e2) => KIf(x, update_types(e1, ty), update_types(e2, ty))
        case KConst(x, v, e) => 
            val (uv, v_ty) = update_val_types(v, ty)
            KConst(x, uv, update_types(e, ty + (x -> uv.get_type)))
        case KStructDef(struct, e) => 
            KStructDef(struct, update_types(e, ty + (struct.name -> UserType(struct.name))))
        case KLetEnv(x, env, e2) => KLetEnv(x, env, update_types(e2, ty + (x -> EnvType(env.name))))
        case KLetEnvRef(x, Ref(env, idx), e2) =>
            // Type overriden by what environment says it is
            KLetEnvRef(x, Ref(KVar(env.s, ty.getOrElse(env.s, EnvType(env.s))), idx), update_types(e2, ty))
        case KWrite(v, in) => KWrite(v, update_types(in, ty))
        case KReturn(v) => KReturn(update_val_types(v, ty)._1)
    }

    def update_val_types(a: KVal, ty: Map[String, Type]): (KVal, Map[String, Type]) = a match {
        case KVar(s, t) =>
            val t2 = ty.getOrElse(s, t)
            (KVar(s, t2), ty + (s -> t2))
        case v => (v, ty)
    }

}

