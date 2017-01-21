
type t

exception Non_consecutive_prefs

val create : int -> int array -> t
val total_preferences : t -> int
