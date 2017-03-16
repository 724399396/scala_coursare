import kmeans.HList.Nat

object test {

  import kmeans.HList.HList._

  val x = 5 :: "Hi" :: true :: HNil

  type X = x.viewAt
}