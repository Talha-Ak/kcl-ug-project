%env_5_t = type { i32 (%env_5_t*, i32)*, i32 }

define %env_5_t* @foo (i32 %x) {
   %bar = alloca %env_5_t
   %bar0 = getelementptr %env_5_t, %env_5_t* %bar, i32 0, i32 0
   store i32 (%env_5_t*, i32)* @bar, i32 (%env_5_t*, i32)** %bar0
   %bar1 = getelementptr %env_5_t, %env_5_t* %bar, i32 0, i32 1
   store i32 %x, i32* %bar1
   ret %env_5_t* %bar
}

define i32 @bar (%env_5_t* %env_5, i32 %y) {
   %ptr_13 = getelementptr %env_5_t, %env_5_t* %env_5, i32 0, i32 1
   %x = load i32, i32* %ptr_13
   %tmp_0 = add i32 %x, %y
   ret i32 %tmp_0
}

define i32 @simple (i32 %a) {
   %tmp_1 = mul i32 %a, 2
   ret i32 %tmp_1
}

define i32 @rec (i32 %x) {
   %tmp_2 = call i32 @rec(i32 1)
   ret i32 %tmp_2
}

define i32 @main () {
   %add = call %env_5_t* @foo(i32 3)
   %ptr_14 = getelementptr %env_5_t, %env_5_t* %add, i32 0, i32 0
   %ptr_8 = load i32 (%env_5_t*, i32)*, i32 (%env_5_t*, i32)** %ptr_14
   %tmp_4 = call i32 %ptr_8(%env_5_t* %add, i32 4)
   ret i32 %tmp_4
}
