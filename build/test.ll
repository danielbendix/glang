; ModuleID = 'test'
source_filename = "test"

define i64 @fac(i64 %0) {
entry:
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 1, ptr %1, align 4
  store i64 2, ptr %2, align 4
  br label %3

3:                                                ; preds = %3, %entry
  %4 = load i64, ptr %1, align 4
  %5 = load i64, ptr %2, align 4
  %6 = mul i64 %4, %5
  store i64 %6, ptr %1, align 4
  %7 = load i64, ptr %2, align 4
  %8 = add i64 %7, 1
  store i64 %8, ptr %2, align 4
  %9 = load i64, ptr %2, align 4
  %10 = icmp slt i64 %9, %0
  br i1 %10, label %3, label %11

11:                                               ; preds = %3
  %12 = load i64, ptr %1, align 4
  ret i64 %12
}

define i64 @fac_rec(i64 %0) {
entry:
  %1 = icmp sgt i64 %0, 1
  br i1 %1, label %2, label %6

2:                                                ; preds = %entry
  %3 = sub i64 %0, 1
  %4 = call i64 @fac(i64 %3)
  %5 = mul i64 %0, %4
  ret i64 %5

6:                                                ; preds = %entry
  ret i64 1
}

define i64 @main() {
entry:
  %0 = alloca i64, align 8
  %1 = alloca i64, align 8
  store i64 42, ptr %0, align 4
  store i64 255, ptr %1, align 4
  %2 = call i64 @fac(i64 10)
  ret i64 %2
}
