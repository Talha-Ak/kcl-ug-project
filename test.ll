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


%Student = type { i32, i1, float }

define %Student* @something (i32 %x) {
    %tmp_0 = alloca %Student
    %tmp_0_0 = getelementptr %Student, %Student* %tmp_0, i32 0, i32 0
    store i32 %x, i32* %tmp_0_0
    %tmp_0_1 = getelementptr %Student, %Student* %tmp_0, i32 0, i32 1
    store i1 true, i1* %tmp_0_1
    %tmp_0_2 = getelementptr %Student, %Student* %tmp_0, i32 0, i32 2
    store float 0x4052c00000000000, float* %tmp_0_2
    ret %Student* %tmp_0
}

define i32 @main () {
    %st = alloca %Student
    %st_0 = getelementptr %Student, %Student* %st, i32 0, i32 0
    store i32 20, i32* %st_0
    %st_1 = getelementptr %Student, %Student* %st, i32 0, i32 1
    store i1 false, i1* %st_1
    %st_2 = getelementptr %Student, %Student* %st, i32 0, i32 2
    store float 0x4056400000000000, float* %st_2
    %ret = call %Student* @something(i32 20)
    call void @print_i32(i32 0)
    %ptr_4 = getelementptr %Student, %Student* %st, i32 0, i32 0
    %el_5 = load i32, i32* %ptr_4
    call void @print_i32(i32 %el_5)
    %ptr_6 = getelementptr %Student, %Student* %st, i32 0, i32 1
    %el_7 = load i1, i1* %ptr_6
    call void @print_i1(i1 %el_7)
    %ptr_8 = getelementptr %Student, %Student* %st, i32 0, i32 2
    %el_9 = load float, float* %ptr_8
    call void @print_float(float %el_9)
    %ptr_10 = getelementptr %Student, %Student* %ret, i32 0, i32 0
    %el_11 = load i32, i32* %ptr_10
    call void @print_i32(i32 %el_11)
    %ptr_12 = getelementptr %Student, %Student* %ret, i32 0, i32 1
    %el_13 = load i1, i1* %ptr_12
    call void @print_i1(i1 %el_13)
    %ptr_14 = getelementptr %Student, %Student* %ret, i32 0, i32 2
    %el_15 = load float, float* %ptr_14
    call void @print_float(float %el_15)
    ret i32 0
}
