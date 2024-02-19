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


define i32 @zero (i32 %x) {
    ret i32 0
}

define i32 @suc (i32 %x) {
    %tmp_0 = add i32 %x, 1
    ret i32 %tmp_0
}

define i32 @pred (i32 %x) {
    %tmp_1 = icmp eq i32 %x, 0
    br i1 %tmp_1, label %if_branch_165, label %else_branch_166

if_branch_165:
    ret i32 %x

else_branch_166:
    %tmp_2 = sub i32 %x, 1
    ret i32 %tmp_2
}

define i32 @add (i32 %x, i32 %y) {
    %tmp_3 = icmp eq i32 %x, 0
    br i1 %tmp_3, label %if_branch_167, label %else_branch_168

if_branch_167:
    ret i32 %y

else_branch_168:
    %tmp_4 = sub i32 %x, 1
    %tmp_5 = call i32 @add(i32 %tmp_4, i32 %y)
    %tmp_6 = call i32 @suc(i32 %tmp_5)
    ret i32 %tmp_6
}

define i32 @mult (i32 %x, i32 %y) {
    %tmp_7 = icmp eq i32 %x, 0
    br i1 %tmp_7, label %if_branch_169, label %else_branch_170

if_branch_169:
    ret i32 0

else_branch_170:
    %tmp_8 = sub i32 %x, 1
    %tmp_9 = call i32 @mult(i32 %tmp_8, i32 %y)
    %tmp_10 = call i32 @add(i32 %y, i32 %tmp_9)
    ret i32 %tmp_10
}

define i32 @pow (i32 %x, i32 %y) {
    %tmp_11 = icmp eq i32 %y, 0
    br i1 %tmp_11, label %if_branch_171, label %else_branch_172

if_branch_171:
    ret i32 1

else_branch_172:
    %tmp_12 = sub i32 %y, 1
    %tmp_13 = call i32 @pow(i32 %x, i32 %tmp_12)
    %tmp_14 = call i32 @mult(i32 %x, i32 %tmp_13)
    ret i32 %tmp_14
}

define i32 @fib (i32 %n) {
    %tmp_15 = icmp eq i32 %n, 0
    br i1 %tmp_15, label %if_branch_173, label %else_branch_174

if_branch_173:
    ret i32 0

else_branch_174:
    %tmp_16 = icmp eq i32 %n, 1
    br i1 %tmp_16, label %if_branch_175, label %else_branch_176

if_branch_175:
    ret i32 1

else_branch_176:
    %tmp_18 = sub i32 %n, 1
    %tmp_19 = call i32 @fib(i32 %tmp_18)
    %tmp_20 = sub i32 %n, 2
    %tmp_21 = call i32 @fib(i32 %tmp_20)
    %tmp_17 = add i32 %tmp_19, %tmp_21
    ret i32 %tmp_17
}

define i32 @fact (i32 %n) {
    %tmp_22 = icmp eq i32 %n, 0
    br i1 %tmp_22, label %if_branch_177, label %else_branch_178

if_branch_177:
    ret i32 1

else_branch_178:
    %tmp_24 = sub i32 %n, 1
    %tmp_25 = call i32 @fact(i32 %tmp_24)
    %tmp_23 = mul i32 %n, %tmp_25
    ret i32 %tmp_23
}

define i32 @ack (i32 %m, i32 %n) {
    %tmp_26 = icmp eq i32 %m, 0
    br i1 %tmp_26, label %if_branch_179, label %else_branch_180

if_branch_179:
    %tmp_27 = add i32 %n, 1
    ret i32 %tmp_27

else_branch_180:
    %tmp_28 = icmp eq i32 %n, 0
    br i1 %tmp_28, label %if_branch_181, label %else_branch_182

if_branch_181:
    %tmp_29 = sub i32 %m, 1
    %tmp_30 = call i32 @ack(i32 %tmp_29, i32 1)
    ret i32 %tmp_30

else_branch_182:
    %tmp_31 = sub i32 %m, 1
    %tmp_32 = sub i32 %n, 1
    %tmp_33 = call i32 @ack(i32 %m, i32 %tmp_32)
    %tmp_34 = call i32 @ack(i32 %tmp_31, i32 %tmp_33)
    ret i32 %tmp_34
}

define i32 @stack_test (i32 %x) {
    %tmp_39 = add i32 %x, 1
    %tmp_38 = add i32 %tmp_39, 2
    %tmp_37 = add i32 %tmp_38, 3
    %tmp_36 = add i32 %tmp_37, 4
    %tmp_35 = add i32 %tmp_36, 5
    ret i32 %tmp_35
}

define i32 @div (i32 %x, i32 %y) {
    %tmp_40 = sdiv i32 %x, %y
    ret i32 %tmp_40
}

define i32 @rem (i32 %x, i32 %y) {
    %tmp_41 = srem i32 %x, %y
    ret i32 %tmp_41
}

define i32 @gcd (i32 %a, i32 %b) {
    %tmp_42 = icmp eq i32 %b, 0
    br i1 %tmp_42, label %if_branch_183, label %else_branch_184

if_branch_183:
    ret i32 %a

else_branch_184:
    %tmp_43 = srem i32 %a, %b
    %tmp_44 = call i32 @gcd(i32 %b, i32 %tmp_43)
    ret i32 %tmp_44
}

define i32 @is_prime_aux (i32 %n, i32 %i) {
    %tmp_46 = srem i32 %n, %i
    %tmp_45 = icmp eq i32 %tmp_46, 0
    br i1 %tmp_45, label %if_branch_185, label %else_branch_186

if_branch_185:
    ret i32 0

else_branch_186:
    %tmp_48 = mul i32 %i, %i
    %tmp_47 = icmp sle i32 %tmp_48, %n
    br i1 %tmp_47, label %if_branch_187, label %else_branch_188

if_branch_187:
    %tmp_49 = add i32 %i, 1
    %tmp_50 = call i32 @is_prime_aux(i32 %n, i32 %tmp_49)
    ret i32 %tmp_50

else_branch_188:
    ret i32 1
}

define i32 @is_prime (i32 %n) {
    %tmp_51 = icmp eq i32 %n, 2
    br i1 %tmp_51, label %if_branch_189, label %else_branch_190

if_branch_189:
    ret i32 1

else_branch_190:
    %tmp_52 = call i32 @is_prime_aux(i32 %n, i32 2)
    ret i32 %tmp_52
}

define i32 @primes (i32 %n) {
    %tmp_53 = icmp eq i32 %n, 0
    br i1 %tmp_53, label %if_branch_191, label %else_branch_192

if_branch_191:
    ret i32 0

else_branch_192:
    %tmp_55 = call i32 @is_prime(i32 %n)
    %tmp_54 = icmp eq i32 %tmp_55, 1
    br i1 %tmp_54, label %if_branch_193, label %else_branch_194

if_branch_193:
    call void @print_i32(i32 %n)
    %tmp_56 = sub i32 %n, 1
    %tmp_57 = call i32 @primes(i32 %tmp_56)
    ret i32 %tmp_57

else_branch_194:
    %tmp_58 = sub i32 %n, 1
    %tmp_59 = call i32 @primes(i32 %tmp_58)
    ret i32 %tmp_59
}

define i32 @is_collatz (i32 %n) {
    %tmp_60 = icmp eq i32 %n, 1
    br i1 %tmp_60, label %if_branch_195, label %else_branch_196

if_branch_195:
    ret i32 1

else_branch_196:
    %tmp_62 = srem i32 %n, 2
    %tmp_61 = icmp eq i32 %tmp_62, 0
    br i1 %tmp_61, label %if_branch_197, label %else_branch_198

if_branch_197:
    %tmp_63 = sdiv i32 %n, 2
    %tmp_64 = call i32 @is_collatz(i32 %tmp_63)
    ret i32 %tmp_64

else_branch_198:
    %tmp_66 = mul i32 3, %n
    %tmp_65 = add i32 %tmp_66, 1
    %tmp_67 = call i32 @is_collatz(i32 %tmp_65)
    ret i32 %tmp_67
}

define i32 @collatz_aux (i32 %n, i32 %i) {
    %tmp_68 = icmp sgt i32 %i, %n
    br i1 %tmp_68, label %if_branch_199, label %else_branch_200

if_branch_199:
    ret i32 0

else_branch_200:
    %tmp_70 = call i32 @is_collatz(i32 %i)
    %tmp_69 = icmp eq i32 %tmp_70, 1
    br i1 %tmp_69, label %if_branch_201, label %else_branch_202

if_branch_201:
    call void @print_i32(i32 %i)
    %tmp_71 = add i32 %i, 1
    %tmp_72 = call i32 @collatz_aux(i32 %n, i32 %tmp_71)
    ret i32 %tmp_72

else_branch_202:
    %tmp_73 = add i32 %i, 1
    %tmp_74 = call i32 @collatz_aux(i32 %n, i32 %tmp_73)
    ret i32 %tmp_74
}

define i32 @collatz (i32 %n) {
    %tmp_75 = call i32 @collatz_aux(i32 %n, i32 1)
    ret i32 %tmp_75
}

define i32 @facT (i32 %n, i32 %acc) {
    %tmp_76 = icmp eq i32 %n, 0
    br i1 %tmp_76, label %if_branch_203, label %else_branch_204

if_branch_203:
    ret i32 %acc

else_branch_204:
    %tmp_77 = sub i32 %n, 1
    %tmp_78 = mul i32 %n, %acc
    %tmp_79 = call i32 @facT(i32 %tmp_77, i32 %tmp_78)
    ret i32 %tmp_79
}

define i32 @main () {
    %tmp_80 = call i32 @ack(i32 3, i32 12)
    call void @print_i32(i32 %tmp_80)
    ret i32 0
}
