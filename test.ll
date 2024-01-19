%env_5_t = type { i32 (%env_5_t*, i32)*, i32 }

define i32 @foo (%x_t* %x, i32 %) {
   %bar = alloca %env_5_t
   %bar0 = getelementptr %env_5_t, %env_5_t* %bar, i32 0, i32 0
   store i32 (%env_5_t*, i32)* @bar, i32 (%env_5_t*, i32)** %bar0
   %bar1 = getelementptr %env_5_t, %env_5_t* %bar, i32 0, i32 1
   store i32 %x, i32* %bar1
   ret i32 %bar
}

define i32 @bar (%env_5_t* %env_5, i32 %y) {
   %x = env ref %env_5 idx 1
   %tmp_0 = add i32 %x, %y
   ret i32 %tmp_0
}

define i32 @simple (%a_t* %a, i32 %) {
   %tmp_1 = mul i32 %a, 2
   ret i32 %tmp_1
}

define i32 @main () {
   %add = call i32 @foo(i32 3)
   %ptr_8 = env ref %add idx 0
   %tmp_3 = call i32 @ptr_8(i32 %add, i32 4)
   ret i32 %tmp_3
}
