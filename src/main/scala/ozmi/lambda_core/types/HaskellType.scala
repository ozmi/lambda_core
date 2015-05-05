package ozmi.lambda_core.types

import scala.util.{Failure, Success, Try}

/**
 * Created by attila on 5/2/2015.
 */
object HaskellType {

    type Id = String

    sealed trait Kind
    case object Star extends Kind
    case class Kfun (kind1 : Kind, kind2 : Kind) extends Kind

    sealed trait Type
    case class TVar (tyvar : Tyvar) extends Type
    case class TCon (tycon : Tycon) extends Type
    case class TAp (type1 : Type, type2 : Type) extends Type
    case class TGen (id : Int) extends Type

    case class Tyvar (id : Id, kind : Kind)

    case class Tycon (id : Id, kind : Kind)

    // Type constants
    val tUnit = TCon (Tycon ("()", Star))
    val tChar = TCon (Tycon ("Char", Star))
    val tInt = TCon (Tycon ("Int", Star))
    val tInteger = TCon (Tycon ("Integer", Star))
    val tFloat = TCon (Tycon ("Float", Star))
    val tDouble = TCon (Tycon ("Double", Star))

    val tList = TCon (Tycon ("[]", Kfun (Star, Star)))
    val tArrow = TCon (Tycon ("(->)", Kfun (Star, (Kfun (Star, Star)))))
    val tTuple2 = TCon (Tycon ("(,)", Kfun (Star, (Kfun (Star, Star)))))

    // utility functions
    def fn (a : Type, b : Type) : Type =
        TAp (TAp (tArrow, a), b)

    def list (t : Type) : Type =
        TAp (tList, t)

    def pair (a : Type, b : Type) : Type =
        TAp (TAp (tTuple2, a), b)

    // HasKind type-class
    trait HasKind {
        def kind : Kind
    }
    implicit class HasKindTyvar (tyvar : Tyvar) extends HasKind {
        def kind : Kind = tyvar.kind
    }
    implicit class HasKindTycon (tycon : Tycon) extends HasKind {
        def kind : Kind = tycon.kind
    }
    implicit class HasKindType (tpe : Type) extends HasKind {
        def kind : Kind = tpe match {
            case TCon (tc) => tc.kind
            case TVar (u) => u.kind
            case TAp (t, _) => t.kind match {
                case Kfun (_, k) => k
            }
        }
    }

    type Subst = Seq[(Tyvar, Type)]

    val nullSubst: Subst = Seq ()

    implicit class RichTyvar (u : Tyvar) {

        def +-> (t: Type): Subst = Seq (u -> t)

    }

    trait Types[T] {
        def apply (s : Subst) : T
        def tv () : Seq[Tyvar]
    }
    implicit class TypesType (tpe : Type) extends Types[Type] {
        def apply (s : Subst) : Type =
            (s, tpe) match {
                case (s, TVar (u))      =>
                    lookup (u, s) match {
                        case Some (t)   => t
                        case None       => TVar(u)
                    }
                case (s, TAp (l, r))    => TAp (l apply s, r apply s)
                case (s, t)             => t
            }
        def tv () : Seq[Tyvar] =
            tpe match {
                case TVar (u)   => Seq (u)
                case TAp (l, r) => l.tv () union r.tv ()
                case t          => Seq ()
            }
    }
    implicit class TypesSeq (tSeq : Seq[Type]) extends Types[Seq[Type]] {
        def apply (s : Subst) : Seq[Type] = tSeq map {t => t apply s}
        def tv () : Seq[Tyvar] = (tSeq flatMap {t => t.tv ()}).distinct
    }

    def lookup (tyvar : Tyvar, s : Subst) : Option[Type] = ???

    implicit class RichSubst (s1 : Subst) {

        def @@ (s2: Subst): Subst =
            (for ((u, t) <- s2) yield (u, t apply s1)) ++ s1

    }

    def merge (s1 : Subst, s2 : Subst) : Try[Subst] = {
        val agree =
            ((s1 map {_._1}) intersect (s2 map {_._1}))
                .forall{v => (TVar (v) apply s1) == (TVar (v) apply s2)}
        if (agree) Success (s1 ++ s2)
        else Failure (sys.error (s"merge failed for ($s1, $s2)"))
    }

    def varBind (u : Tyvar, t : Type) : Try[Subst] = (u, t) match {
        case (u, t) if t == TVar (u)        => Success (nullSubst)
        case (u, t) if t.tv() contains u    => Failure (sys.error (s"occurs check failed on ($u, $t)"))
        case (u, t) if u.kind != t.kind     => Failure (sys.error (s"kinds do not match on ($u, $t)"))
        case (u, t)                         => Success (u +-> t)
    }

    def mgu (t1 : Type, t2 : Type) : Try[Subst] = (t1, t2) match {
        case (TAp (l, r), TAp (lp, rp)) =>
            for {
                s1 <- mgu (l, lp)
                s2 <- mgu (r apply s1, rp apply s1)
            } yield (s2 @@ s1)
        case (TVar (u), t) => varBind (u, t)
        case (t, TVar (u)) => varBind (u, t)
        case (TCon (tc1), TCon (tc2)) if tc1 == tc2 => Success (nullSubst)
        case (t1, t2) => Failure (sys.error (s"types ($t1, $t2) do not unify"))
    }

    def matc (t1 : Type, t2 : Type) : Try[Subst] = (t1, t2) match {
        case (TAp (l, r), TAp (lp, rp)) =>
            for {
                sl <- matc (l, lp)
                sr <- matc (r, rp)
            } yield merge (sl, sr).get
        case (TVar (u), t) if u.kind == t.kind => Success (u +-> t)
        case (TCon (tc1), TCon (tc2)) if tc1 == tc2 => Success (nullSubst)
        case (t1, t2) => Failure (sys.error (s"types ($t1, $t2) do not match"))
    }


}
