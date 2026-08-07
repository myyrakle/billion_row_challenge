package measurement

import "time"

type Timer struct {
	start int64
}

func NewTimer() Timer {
	return Timer{
		start: time.Now().UnixNano(),
	}
}

func (t *Timer) Elapsed() time.Duration {
	return time.Duration(time.Now().UnixNano() - t.start)
}

func (t *Timer) ElapsedAsSeconds() float64 {
	return t.Elapsed().Seconds()
}

func (t *Timer) ElapsedAsMilliseconds() float64 {
	return t.Elapsed().Seconds() * 1000
}
