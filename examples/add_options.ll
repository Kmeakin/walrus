; ModuleID = 'examples/add_options'
source_filename = "examples/add_options"

%Option = type { i8, { i32 } }

define void @builtins.exit.wrapper(i8* %env, i32 %status) {
  call void @exit(i32 %status)
  unreachable
}

declare void @exit(i32)

define i32 @builtins.putchar.wrapper(i8* %env, i32 %c) {
  %1 = call i32 @putchar(i32 %c)
  ret i32 %1
}

declare i32 @putchar(i32)

define %Option @add_options(i8* %add_options.env, %Option %add_options.params.0, %Option %add_options.params.1) {
add_options.entry:
  %a.alloca = alloca %Option, align 8
  store %Option %add_options.params.0, %Option* %a.alloca, align 4
  %b.alloca = alloca %Option, align 8
  store %Option %add_options.params.1, %Option* %b.alloca, align 4
  %tuple.alloca = alloca { %Option, %Option }, align 8
  %a = load %Option, %Option* %a.alloca, align 4
  %tuple.0.gep = getelementptr inbounds { %Option, %Option }, { %Option, %Option }* %tuple.alloca, i32 0, i32 0
  store %Option %a, %Option* %tuple.0.gep, align 4
  %b = load %Option, %Option* %b.alloca, align 4
  %tuple.1.gep = getelementptr inbounds { %Option, %Option }, { %Option, %Option }* %tuple.alloca, i32 0, i32 1
  store %Option %b, %Option* %tuple.1.gep, align 4
  %tuple = load { %Option, %Option }, { %Option, %Option }* %tuple.alloca, align 4
  br label %match.case0.test

match.case0.test:                                 ; preds = %add_options.entry
  %tuple.0 = extractvalue { %Option, %Option } %tuple, 0
  %Option.discriminant = extractvalue %Option %tuple.0, 0
  %"Option::Some.cmp_discriminant" = icmp eq i8 %Option.discriminant, 1
  br i1 %"Option::Some.cmp_discriminant", label %"match.case0.Option::Some.then", label %"match.case0.Option::Some.else"

"match.case0.Option::Some.then":                  ; preds = %match.case0.test
  %Option.payload = extractvalue %Option %tuple.0, 1
  %"Option::Some.x" = extractvalue { i32 } %Option.payload, 0
  br label %"match.case0.Option::Some.end"

"match.case0.Option::Some.else":                  ; preds = %match.case0.test
  br label %"match.case0.Option::Some.end"

"match.case0.Option::Some.end":                   ; preds = %"match.case0.Option::Some.then", %"match.case0.Option::Some.else"
  %"match.case0.Option::Some.phi" = phi i1 [ true, %"match.case0.Option::Some.then" ], [ false, %"match.case0.Option::Some.else" ]
  %0 = and i1 true, %"match.case0.Option::Some.phi"
  %tuple.1 = extractvalue { %Option, %Option } %tuple, 1
  %Option.discriminant1 = extractvalue %Option %tuple.1, 0
  %"Option::Some.cmp_discriminant2" = icmp eq i8 %Option.discriminant1, 1
  br i1 %"Option::Some.cmp_discriminant2", label %"match.case1.Option::Some.then", label %"match.case1.Option::Some.else"

"match.case1.Option::Some.then":                  ; preds = %"match.case0.Option::Some.end"
  %Option.payload3 = extractvalue %Option %tuple.1, 1
  %"Option::Some.x4" = extractvalue { i32 } %Option.payload3, 0
  br label %"match.case1.Option::Some.end"

"match.case1.Option::Some.else":                  ; preds = %"match.case0.Option::Some.end"
  br label %"match.case1.Option::Some.end"

"match.case1.Option::Some.end":                   ; preds = %"match.case1.Option::Some.then", %"match.case1.Option::Some.else"
  %"match.case1.Option::Some.phi" = phi i1 [ true, %"match.case1.Option::Some.then" ], [ false, %"match.case1.Option::Some.else" ]
  %1 = and i1 %0, %"match.case1.Option::Some.phi"
  br i1 %1, label %match.case0.then, label %match.case1.test

match.case0.then:                                 ; preds = %"match.case1.Option::Some.end"
  %tuple.05 = extractvalue { %Option, %Option } %tuple, 0
  %Option.payload6 = extractvalue %Option %tuple.05, 1
  %"Option::Some.x7" = extractvalue { i32 } %Option.payload6, 0
  %a.alloca8 = alloca i32, align 4
  store i32 %"Option::Some.x7", i32* %a.alloca8, align 4
  %tuple.19 = extractvalue { %Option, %Option } %tuple, 1
  %Option.payload10 = extractvalue %Option %tuple.19, 1
  %"Option::Some.x11" = extractvalue { i32 } %Option.payload10, 0
  %b.alloca12 = alloca i32, align 4
  store i32 %"Option::Some.x11", i32* %b.alloca12, align 4
  %Option.alloca13 = alloca %Option, align 8
  %Option.discriminant.gep14 = getelementptr inbounds %Option, %Option* %Option.alloca13, i32 0, i32 0
  store i8 1, i8* %Option.discriminant.gep14, align 1
  %Option.payload.gep15 = getelementptr inbounds %Option, %Option* %Option.alloca13, i32 0, i32 1
  %a16 = load i32, i32* %a.alloca8, align 4
  %b17 = load i32, i32* %b.alloca12, align 4
  %2 = add i32 %a16, %b17
  %"Option::Some.x.gep" = getelementptr inbounds { i32 }, { i32 }* %Option.payload.gep15, i32 0, i32 0
  store i32 %2, i32* %"Option::Some.x.gep", align 4
  %Option.load18 = load %Option, %Option* %Option.alloca13, align 4
  br label %match.end

match.case1.test:                                 ; preds = %"match.case1.Option::Some.end"
  br i1 true, label %match.case1.then, label %match.fail

match.case1.then:                                 ; preds = %match.case1.test
  %Option.alloca = alloca %Option, align 8
  %Option.discriminant.gep = getelementptr inbounds %Option, %Option* %Option.alloca, i32 0, i32 0
  store i8 0, i8* %Option.discriminant.gep, align 1
  %Option.payload.gep = getelementptr inbounds %Option, %Option* %Option.alloca, i32 0, i32 1
  %Option.load = load %Option, %Option* %Option.alloca, align 4
  br label %match.end

match.fail:                                       ; preds = %match.case1.test
  unreachable

match.end:                                        ; preds = %match.case0.then, %match.case1.then
  %match.phi = phi %Option [ %Option.load, %match.case1.then ], [ %Option.load18, %match.case0.then ]
  ret %Option %match.phi
}

define %Option @main(i8* %main.env) {
main.entry:
  %Option.alloca = alloca %Option, align 8
  %Option.discriminant.gep = getelementptr inbounds %Option, %Option* %Option.alloca, i32 0, i32 0
  store i8 1, i8* %Option.discriminant.gep, align 1
  %Option.payload.gep = getelementptr inbounds %Option, %Option* %Option.alloca, i32 0, i32 1
  %"Option::Some.x.gep" = getelementptr inbounds { i32 }, { i32 }* %Option.payload.gep, i32 0, i32 0
  store i32 1, i32* %"Option::Some.x.gep", align 4
  %Option.load = load %Option, %Option* %Option.alloca, align 4
  %a.alloca = alloca %Option, align 8
  store %Option %Option.load, %Option* %a.alloca, align 4
  %Option.alloca1 = alloca %Option, align 8
  %Option.discriminant.gep2 = getelementptr inbounds %Option, %Option* %Option.alloca1, i32 0, i32 0
  store i8 1, i8* %Option.discriminant.gep2, align 1
  %Option.payload.gep3 = getelementptr inbounds %Option, %Option* %Option.alloca1, i32 0, i32 1
  %"Option::Some.x.gep4" = getelementptr inbounds { i32 }, { i32 }* %Option.payload.gep3, i32 0, i32 0
  store i32 2, i32* %"Option::Some.x.gep4", align 4
  %Option.load5 = load %Option, %Option* %Option.alloca1, align 4
  %b.alloca = alloca %Option, align 8
  store %Option %Option.load5, %Option* %b.alloca, align 4
  %add_options.closure.alloca = alloca { %Option (i8*, %Option, %Option)*, i8* }, align 8
  %add_options.closure.code = getelementptr inbounds { %Option (i8*, %Option, %Option)*, i8* }, { %Option (i8*, %Option, %Option)*, i8* }* %add_options.closure.alloca, i32 0, i32 0
  store %Option (i8*, %Option, %Option)* @add_options, %Option (i8*, %Option, %Option)** %add_options.closure.code, align 8
  %add_options.closure.env = getelementptr inbounds { %Option (i8*, %Option, %Option)*, i8* }, { %Option (i8*, %Option, %Option)*, i8* }* %add_options.closure.alloca, i32 0, i32 1
  store i8* null, i8** %add_options.closure.env, align 8
  %add_options = load { %Option (i8*, %Option, %Option)*, i8* }, { %Option (i8*, %Option, %Option)*, i8* }* %add_options.closure.alloca, align 8
  %closure.code = extractvalue { %Option (i8*, %Option, %Option)*, i8* } %add_options, 0
  %closure.env = extractvalue { %Option (i8*, %Option, %Option)*, i8* } %add_options, 1
  %a = load %Option, %Option* %a.alloca, align 4
  %b = load %Option, %Option* %b.alloca, align 4
  %call = call %Option %closure.code(i8* %closure.env, %Option %a, %Option %b)
  ret %Option %call
}
