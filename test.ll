%env_6_t = type { i32 (%env_6_t*, i32)*, i32 }

define %env_6_t @foo (i32 %x) {
   %bar = alloca %env_6_t
   %bar0 = getelementptr %env_6_t, %env_6_t* %bar, i32 0, i32 0
   store i32 (%env_6_t*, i32)* @bar, i32 (%env_6_t*, i32)** %bar0
   %bar1 = getelementptr %env_6_t, %env_6_t* %bar, i32 0, i32 1
   store i32 %x, i32* %bar1
   ret i32 %bar
}

define i32 @bar (%env_6_t %env_6, i32 %y) {
   %x = env ref %env_6 idx 1
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
   %add = call i32 @foo(i32 3)
   %ptr_11 = env ref %add idx 0
   %tmp_4 = call i32 @ptr_11(i32 %add, i32 4)
   ret i32 %tmp_4
}
