package json

import (
	"errors"
	"reflect"
)

type typedMarshaler struct {
	typ     reflect.Type
	fnc     marshaler
}

func MarshalFuncV1[T any](fn func(T) ([]byte, error)) typedMarshaler {
	t := reflect.TypeOf((*T)(nil)).Elem()
	return typedMarshaler{
		typ: t,
		fnc: func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			val, err := fn(va.Convert(t).Interface().(T))
			if err != nil {
				if err == SkipFunc {
					err = errors.New("marshal function of type func(T) ([]byte, error) cannot be skipped")
				}
				// TODO: Avoid wrapping semantic errors.
				return &SemanticError{action: "marshal", GoType: t, Err: err}
			}
			if err := enc.WriteValue(val); err != nil {
				// TODO: Avoid wrapping semantic or I/O errors.
				return &SemanticError{action: "marshal", JSONKind: RawValue(val).Kind(), GoType: t, Err: err}
			}
			return nil
		},
	}
}

func init() {
	NewMarshalers(
		MarshalFuncV1(func(string) ([]byte, error) {
			return []byte("hello"), nil
		}),
	)
}
