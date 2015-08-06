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
// built-in functions
// int
func Plus(objs ...CloverObj) CloverObj {
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

func Minus(objs ...CloverObj) CloverObj {
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

func Mul(objs ...CloverObj) CloverObj {
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
func And(objs ...CloverObj) CloverObj {
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

func Or(objs ...CloverObj) CloverObj {
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

func Not(objs ...CloverObj) CloverObj {
	o := objs[0]
	v := o.(CloverBool).value
	r := CloverBool{!v}
	return r
}

// util
func println(objs ...CloverObj) CloverObj {
	for _, v := range objs {
		fmt.Println(v.ShowValue())
		}
	return CloverNil{0}
}

func print(objs ...CloverObj) CloverObj {
	for _, v := range objs {
		fmt.Print(v.ShowValue())
		}
	return CloverNil{0}
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

func Eq(objs ...CloverObj) CloverObj {
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

func Neq(objs ...CloverObj) CloverObj {
	return Not(Eq(objs...))
}

func Gr(objs ...CloverObj) CloverObj {
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

func Le(objs ...CloverObj) CloverObj {
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



func Gre(objs ...CloverObj) CloverObj {
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

func Lee(objs ...CloverObj) CloverObj {
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


func tes(x, y interface{}) CloverObj {
	v1 := x.(CloverInt)
	v2 := y.(CloverInt)
	return Plus(v1, v2)
}

func If(b CloverObj, t, f func(...CloverObj) CloverObj) CloverObj {
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
