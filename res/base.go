package main

import (
	"fmt"
)

type CloverInt struct {
	value int
}

type CloverFloat struct {
	value float64
}

type CloverBool struct {
	value bool
}

type CloverString struct {
	value string
}

type CloverNil struct {
	value int
}

type CloverVector struct {
	value []CloverObj
}

type CloverFunc struct {
	value func(...interface{}) CloverObj
}

type CloverObj interface {
	ShowValue() string
}

func (s CloverInt) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverFloat) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverBool) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverString) ShowValue() string {
	return fmt.Sprint(s.value)
}

func (s CloverNil) ShowValue() string {
	return "nil"
}

func (s CloverVector) ShowValue() string {
	vs := s.value
	r := make([]string, len(vs))
	for i, v := range vs {
		r[i] = v.ShowValue()
	}
	return fmt.Sprint(r)
}

func (s CloverFunc) ShowValue() string {
	return "#function"
}
// built-in functions
// int
var Plus CloverObj = CloverFunc{PlusFunc}
var Minus CloverObj = CloverFunc{MinusFunc}
var Mul CloverObj = CloverFunc{MulFunc}

func PlusFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	init := CloverInt{0}
	var sum CloverObj
	for i, v := range objs {
		if i == 0 {
			sum = pluser(init, v)
		} else {
		sum = pluser(sum, v)
		}
	}
	return sum
}

func pluser(o1, o2 CloverObj) CloverObj {
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverInt{(o1.(CloverInt).value + o2.(CloverInt).value)}
	case b1 == b2:
		return CloverFloat{(o1.(CloverFloat).value + o2.(CloverFloat).value)}
	case b1:
		return CloverFloat{(float64(o1.(CloverInt).value) + o2.(CloverFloat).value)}
	case b2:
		return CloverFloat{o1.(CloverFloat).value + (float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

func MinusFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	init := CloverInt{0}
	var sum CloverObj
	for i, v := range objs {
		if i == 0 {
			sum = minuser(init, v)
		} else {
		sum = minuser(sum, v)
		}
	}
	return sum
}

func minuser(o1, o2 CloverObj) CloverObj {
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverInt{(o1.(CloverInt).value - o2.(CloverInt).value)}
	case b1 == b2:
		return CloverFloat{(o1.(CloverFloat).value - o2.(CloverFloat).value)}
	case b1:
		return CloverFloat{(float64(o1.(CloverInt).value) - o2.(CloverFloat).value)}
	case b2:
		return CloverFloat{o1.(CloverFloat).value - (float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

func MulFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	init := CloverInt{1}
	var sum CloverObj
	for i, v := range objs {
		if i == 0 {
			sum = muler(init, v)
		} else {
		sum = muler(sum, v)
		}
	}
	return sum
}

func muler(o1, o2 CloverObj) CloverObj {
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverInt{(o1.(CloverInt).value * o2.(CloverInt).value)}
	case b1 == b2:
		return CloverFloat{(o1.(CloverFloat).value * o2.(CloverFloat).value)}
	case b1:
		return CloverFloat{(float64(o1.(CloverInt).value) * o2.(CloverFloat).value)}
	case b2:
		return CloverFloat{o1.(CloverFloat).value * (float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

// bool
var And CloverObj = CloverFunc{AndFunc}
var Or CloverObj = CloverFunc{OrFunc}
var Not CloverObj = CloverFunc{NotFunc}

func AndFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := objs[0]
	for _, v := range objs {
		res = ander(v, res)
	}
	return res
}

func ander(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverBool).value
	v2 := o2.(CloverBool).value
	o := CloverBool{v1 && v2}
	return o
}

func OrFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := objs[0]
	for _, v := range objs {
		res = orer(v, res)
	}
	return res
}

func orer(o1, o2 CloverObj) CloverObj {
	v1 := o1.(CloverBool).value
	v2 := o2.(CloverBool).value
	o := CloverBool{v1 || v2}
	return o
}

func NotFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	t := objs[0]
	v := t.(CloverBool).value
	r := CloverBool{!v}
	return r
}

// comparsion operator
var Eq CloverObj = CloverFunc{EqFunc}
var Gr CloverObj = CloverFunc{GrFunc}
var Le CloverObj = CloverFunc{LeFunc}
var Gre CloverObj = CloverFunc{GreFunc}
var Lee CloverObj = CloverFunc{LeeFunc}

func EqFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
  res := true
	init := objs[0]
	for _, v := range objs {
		if res {
			res = eqer(v, init)
		} else {
			return CloverBool{false}
		}
	}
	return CloverBool{res}
}

func eqer(o1, o2 CloverObj) bool {
	switch o1.(type) {
		case CloverInt:
			return (o1.(CloverInt).value == o2.(CloverInt).value)
		case CloverFloat:
			return (o1.(CloverFloat).value == o2.(CloverFloat).value)
		case CloverString:
			return (o1.(CloverString).value == o2.(CloverString).value)
		case CloverBool:
			return (o1.(CloverBool).value == o2.(CloverBool).value)
	}
	return false
}

func NeqFunc(o ...interface{}) CloverObj {
	return NotFunc(EqFunc(o))
}

func GrFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := true
	for i, v := range objs {
		if !res {
			return CloverBool{false}
		}
		if i < len(objs) - 1 {
			res = grer(v, objs[i+1])
		} else {
			return CloverBool{res}
		}
	}
	return CloverString{"Error!"}
}

func grer(o1, o2 CloverObj) bool {
	b1 := intp(o1)
	b2 := intp(o2)
	b3 := strp(o1)
	b4 := strp(o2)
	switch {
	case b1 && b2:
		return (o1.(CloverInt).value > o2.(CloverInt).value)
	case b1 == b2:
		return (o1.(CloverFloat).value > o2.(CloverFloat).value)
	case b1:
		return (float64(o1.(CloverInt).value) > o2.(CloverFloat).value)
	case b2:
		return (o1.(CloverFloat).value > float64(o2.(CloverInt).value))
	case b3 && b4:
		return (o1.(CloverString).value > (o2.(CloverString).value))
	}
	return false
}

func LeFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := true
	for i, v := range objs {
		if !res {
			return CloverBool{false}
		}
		if i < len(objs) - 1 {
			res = leer(v, objs[i+1])
		} else {
			return CloverBool{res}
		}
	}
	return CloverString{"Error!"}
}

func leer(o1, o2 CloverObj) bool {
	b1 := intp(o1)
	b2 := intp(o2)
	b3 := strp(o1)
	b4 := strp(o2)
	switch {
	case b1 && b2:
		return (o1.(CloverInt).value < o2.(CloverInt).value)
	case b1 == b2:
		return (o1.(CloverFloat).value < o2.(CloverFloat).value)
	case b1:
		return (float64(o1.(CloverInt).value) < o2.(CloverFloat).value)
	case b2:
		return (o1.(CloverFloat).value < float64(o2.(CloverInt).value))
	case b3 && b4:
		return (o1.(CloverString).value < (o2.(CloverString).value))
	}
	return false
}

func GreFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := true
	for i, v := range objs {
		if !res {
			return CloverBool{false}
		}
		if i < len(objs) - 1 {
			res = greer(v, objs[i+1])
		} else {
			return CloverBool{res}
		}
	}
	return CloverString{"Error!"}
}

func greer(o1, o2 CloverObj) bool {
	b1 := intp(o1)
	b2 := intp(o2)
	b3 := strp(o1)
	b4 := strp(o2)
	switch {
	case b1 && b2:
		return (o1.(CloverInt).value >= o2.(CloverInt).value)
	case b1 == b2:
		return (o1.(CloverFloat).value >= o2.(CloverFloat).value)
	case b1:
		return (float64(o1.(CloverInt).value) >= o2.(CloverFloat).value)
	case b2:
		return (o1.(CloverFloat).value >= float64(o2.(CloverInt).value))
	case b3 && b4:
		return (o1.(CloverString).value >= (o2.(CloverString).value))
	}
	return false
}

func LeeFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	res := true
	for i, v := range objs {
		if !res {
			return CloverBool{false}
		}
		if i < len(objs) - 1 {
			res = leeer(v, objs[i+1])
		} else {
			return CloverBool{res}
		}
	}
	return CloverString{"Error!"}
}

func leeer(o1, o2 CloverObj) bool {
	b1 := intp(o1)
	b2 := intp(o2)
	b3 := strp(o1)
	b4 := strp(o2)
	switch {
	case b1 && b2:
		return (o1.(CloverInt).value <= o2.(CloverInt).value)
	case b1 == b2:
		return (o1.(CloverFloat).value <= o2.(CloverFloat).value)
	case b1:
		return (float64(o1.(CloverInt).value) <= o2.(CloverFloat).value)
	case b2:
		return (o1.(CloverFloat).value <= float64(o2.(CloverInt).value))
	case b3 && b4:
		return (o1.(CloverString).value <= (o2.(CloverString).value))
	}
	return false
}

// util
var println CloverObj  = CloverFunc{printlnFunc}
var print CloverObj  = CloverFunc{printFunc}

func printlnFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	for _, v := range objs {
		fmt.Println(v.ShowValue())
		}
	return CloverNil{0}
}

func printFunc(o ...interface{}) CloverObj {
	objs := preprocess(o)
	for _, v := range objs {
		fmt.Print(v.ShowValue())
		}
	return CloverNil{0}
}

// internal

func preprocess(o []interface{}) []CloverObj {
	r := make([]CloverObj, len(o))
	for i, v := range o {
		switch o[i].(type) {
		case CloverInt:
			r[i] = v.(CloverInt)
		case CloverFloat:
			r[i] = v.(CloverFloat)
		case CloverString:
			r[i] = v.(CloverString)
		case CloverNil:
			r[i] = v.(CloverNil)
		case func(...interface{}) CloverObj:
			r[i] = CloverFunc{v.(func(...interface{}) CloverObj)}
		default:
			r[i] = v.(CloverObj)
		}
	}
	return r
}

func boolp(o CloverObj) bool {
	switch o.(type) {
	case CloverBool:
		return true
	default:
		return false
	}
}

func intp(o CloverObj) bool {
	switch o.(type) {
	case CloverInt:
		return true
	default:
		return false
	}
}

func strp(o CloverObj) bool {
	switch o.(type) {
	case CloverString:
		return true
	default:
		return false
	}
}

func nilp(o CloverObj) bool {
	switch o.(type) {
	case CloverNil:
		return true
	default:
		return false
	}
}

func If(b CloverObj, t, f func(...interface{}) CloverObj) CloverObj {
	var r1, r2 bool
	r1 = boolp(b)
	r2 = nilp(b)
	if r1 {
		if b.(CloverBool).value {
			return t()
		} else {
			return f()
		}
	} else {
		if r2 {
			return f()
		} else {
			return t()
		}
	}
	return CloverString{"Error!"}
}

/*
func main() {
	fmt.Println(Eq(CloverInt{0}, CloverInt{1}))
	fmt.Println(Eq(CloverBool{}, CloverBool{}))
	fmt.Println(Neq(CloverString{"aaa"}, CloverString{"sss"}))
	fmt.Println(Eq(CloverString{"aaa"}, CloverString{"aaa"}))
}
*/
// (defn f-name [x y] (+ x y))
// to
// List[Symbol "defn", Symbol "fname", Vector[Symbol "x", Symbol "y"],
//      List[Symbol "+", Symbol "x", Symbol "y"]
// ]
//
