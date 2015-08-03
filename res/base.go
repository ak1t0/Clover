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

// built-in functions
// int
func Plus(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
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
	o1 := objs[0]
	o2 := objs[1]
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
	o1 := objs[0]
	o2 := objs[1]
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
	o1 := objs[0]
	o2 := objs[1]
	v1 := o1.(CloverBool).value
	v2 := o2.(CloverBool).value
	o := CloverBool{v1 && v2}
	return o
}

func Or(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
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
func println(objs ...CloverObj) {
	o := objs[0]
	fmt.Println(o.ShowValue())
}

func intp(o1 CloverObj) bool {
	switch o1.(type) {
	case CloverInt:
		return true
	default:
		return false
	}
}

func Eq(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	switch o1.(type) {
		case CloverInt:
			return CloverBool{(o1.(CloverInt).value == o2.(CloverInt).value)}
		case CloverFloat:
			return CloverBool{(o1.(CloverFloat).value == o2.(CloverFloat).value)}
		case CloverString:
			return CloverBool{(o1.(CloverString).value == o2.(CloverString).value)}
		case CloverBool:
			return CloverBool{(o1.(CloverBool).value == o2.(CloverBool).value)}
	}
	return CloverString{"Error!"}
}

func Neq(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	return Not(Eq(o1, o2))
}

func Gr(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverBool{(o1.(CloverInt).value > o2.(CloverInt).value)}
	case b1 == b2:
		return CloverBool{(o1.(CloverFloat).value > o2.(CloverFloat).value)}
	case b1:
		return CloverBool{(float64(o1.(CloverInt).value) > o2.(CloverFloat).value)}
	case b2:
		return CloverBool{(o1.(CloverFloat).value > float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

func Le(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverBool{(o1.(CloverInt).value < o2.(CloverInt).value)}
	case b1 == b2:
		return CloverBool{(o1.(CloverFloat).value < o2.(CloverFloat).value)}
	case b1:
		return CloverBool{(float64(o1.(CloverInt).value) < o2.(CloverFloat).value)}
	case b2:
		return CloverBool{(o1.(CloverFloat).value < float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

func Gre(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverBool{(o1.(CloverInt).value >= o2.(CloverInt).value)}
	case b1 == b2:
		return CloverBool{(o1.(CloverFloat).value >= o2.(CloverFloat).value)}
	case b1:
		return CloverBool{(float64(o1.(CloverInt).value) >= o2.(CloverFloat).value)}
	case b2:
		return CloverBool{(o1.(CloverFloat).value >= float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}

func Lee(objs ...CloverObj) CloverObj {
	o1 := objs[0]
	o2 := objs[1]
	b1 := intp(o1)
	b2 := intp(o2)
	switch {
	case b1 && b2:
		return CloverBool{(o1.(CloverInt).value <= o2.(CloverInt).value)}
	case b1 == b2:
		return CloverBool{(o1.(CloverFloat).value <= o2.(CloverFloat).value)}
	case b1:
		return CloverBool{(float64(o1.(CloverInt).value) <= o2.(CloverFloat).value)}
	case b2:
		return CloverBool{(o1.(CloverFloat).value <= float64(o2.(CloverInt).value))}
	}
	return CloverString{"Error!"}
}


func tes(x, y interface{}) CloverObj {
	v1 := x.(CloverInt)
	v2 := y.(CloverInt)
	return Plus(v1, v2)
}

func If(b CloverBool, t, f func(...CloverObj) CloverObj) CloverObj {
	return CloverBool{true}
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
