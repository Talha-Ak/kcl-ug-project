@.str = private constant [4 x i8] c"%d\0A\00"
@.str.1 = private constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

define void @print_i32(i32 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
    ret void
}

define void @print_i1(i1 %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i1 %x) 
    ret void
}

define void @print_float(float %x) {
    %t0 = getelementptr [4 x i8], [4 x i8]* @.str.1, i32 0, i32 0
    %t1 = fpext float %x to double      ; convert float to double
    call i32 (i8*, ...) @printf(i8* %t0, double %t1) 
    ret void
}

; <===== Generated code starts here =====>
%env_11_t = type { float (%env_11_t*, float)*, float }

define %env_11_t* @foo (float %x) {
    %bar = alloca %env_11_t
    %bar0 = getelementptr %env_11_t, %env_11_t* %bar, i32 0, i32 0
    store float (%env_11_t*, float)* @bar, float (%env_11_t*, float)** %bar0
    %bar1 = getelementptr %env_11_t, %env_11_t* %bar, i32 0, i32 1
    store float %x, float* %bar1
    ret %env_11_t* %bar
}

define float @bar (%env_11_t* %env_11, float %y) {
    %ptr_27 = getelementptr %env_11_t, %env_11_t* %env_11, i32 0, i32 1
    %x = load float, float* %ptr_27
    %tmp_0 = fadd float %x, %y
    ret float %tmp_0
}

define i1 @da (i32 %x) {
    %tmp_1 = icmp eq i32 %x, 1
    br i1 %tmp_1, label %if_branch_28, label %else_branch_29

if_branch_28:
    ret i1 true

else_branch_29:
    ret i1 false
}

define i32 @main2 () {
    %add = call %env_11_t* @foo(float 0x3ff0000000000000)
    %test = add i32 0, 0
    %tmp_3 = icmp eq i32 %test, 0
    br i1 %tmp_3, label %if_branch_30, label %else_branch_31

if_branch_30:
    %tmp_4 = icmp eq i32 %test, 0
    br i1 %tmp_4, label %if_branch_32, label %else_branch_33

if_branch_32:
    call void @print_i32(i32 99)
    ret i32 0

else_branch_33:
    %tmp_5 = add i32 0, 0
    call void @print_i32(i32 %tmp_5)
    ret i32 0

else_branch_31:
    %tmp_6 = icmp eq i32 %test, 4
    br i1 %tmp_6, label %if_branch_34, label %else_branch_35

if_branch_34:
    %ptr_36 = getelementptr %env_11_t, %env_11_t* %add, i32 0, i32 0
    %ptr_12 = load float (%env_11_t*, float)*, float (%env_11_t*, float)** %ptr_36
    %tmp_7 = call float %ptr_12(%env_11_t* %add, float 0x4000000000000000)
    call void @print_float(float %tmp_7)
    ret i32 0

else_branch_35:
    call void @print_i32(i32 0)
    ret i32 0
}

define i32 @square (i32 %x) {
    %tmp_8 = mul i32 %x, %x
    ret i32 %tmp_8
}

define i32 @apply (i32 (i32)* %f, i32 %x) {
    %tmp_9 = call i32 %f(i32 %x)
    ret i32 %tmp_9
}

define i32 @main () {
    %output = call i32 @apply(i32 (i32)* @square, i32 10)
    call void @print_i32(i32 0)
    call void @print_i32(i32 %output)
    ret i32 0
}
