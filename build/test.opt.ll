; ModuleID = 'test.opt.ll'
source_filename = "test"

define i64 @fac(i64 %0) {
entry:
  br label %1

1:                                                ; preds = %1, %entry
  %.01 = phi i64 [ 2, %entry ], [ %3, %1 ]
  %.0 = phi i64 [ 1, %entry ], [ %2, %1 ]
  %2 = mul i64 %.0, %.01
  %3 = add i64 %.01, 1
  %4 = icmp slt i64 %3, %0
  br i1 %4, label %1, label %5

5:                                                ; preds = %1
  ret i64 %2
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
  %0 = call i64 @fac(i64 10)
  ret i64 %0
}
