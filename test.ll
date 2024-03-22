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




define i32 @ack (i32 %m, i32 %n) {
    %tmp_0 = icmp eq i32 %m, 0
    br i1 %tmp_0, label %if_branch_15, label %else_branch_16

if_branch_15:
    %tmp_1 = add i32 %n, 1
    ret i32 %tmp_1

else_branch_16:
    %tmp_2 = icmp eq i32 %n, 0
    br i1 %tmp_2, label %if_branch_17, label %else_branch_18

if_branch_17:
    %tmp_3 = sub i32 %m, 1
    %tmp_4 = call i32 @ack(i32 %tmp_3, i32 1)
    ret i32 %tmp_4

else_branch_18:
    %tmp_5 = sub i32 %m, 1
    %tmp_6 = sub i32 %n, 1
    %tmp_7 = call i32 @ack(i32 %m, i32 %tmp_6)
    %tmp_8 = call i32 @ack(i32 %tmp_5, i32 %tmp_7)
    ret i32 %tmp_8
}

define i32 @main () {
    %tmp_9 = call i32 @ack(i32 3, i32 13)
    call void @print_i32(i32 %tmp_9)
    ret i32 0
}
