type 'a dequeue = 'a list * 'a list
val empty : 'a dequeue
val is_empty : 'a dequeue -> bool
val cons : 'a dequeue -> 'a * 'a dequeue
val pop_front : 'a dequeue -> 'a * 'a dequeue
val pop_back : 'a dequeue -> 'a * 'a dequeue
val head : 'a dequeue -> 'a
val tail : 'a dequeue -> 'a dequeue
val push_back : 'a dequeue -> 'a -> 'a dequeue
val push_front : 'a dequeue -> 'a -> 'a dequeue
val length : 'a dequeue -> int
val oflist : 'a list -> 'a dequeue
val remove : 'a dequeue -> 'a -> 'a dequeue
