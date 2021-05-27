package main

import (
	"fmt"
	"log"
	"runtime"
	"sort"
	"unsafe"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

const (
	width  = 800
	height = 800
)

var points []Point
var appendMode, wall, fillMode, filterMode bool
var wallX, maxY, minY int

type Point struct {
	x, y int
}

type Color struct {
	r float32
	g float32
	b float32
}

type MainFrame struct {
	width  int
	height int
	buff   []float32
}

func initFrame(width, height int) *MainFrame {
	f := MainFrame{
		width:  width,
		height: height,
		buff:   nil,
	}
	for i := 0; i < width*height; i++ {
		f.buff = append(f.buff, 0, 0, 0)
	}
	return &f
}

func (f *MainFrame) getInd(x, y int) int {
	return (y*f.width + x) * 3
}

func (f *MainFrame) setPixel(x, y int, color Color) {
	ind := f.getInd(x, y)
	if ind < 0 || ind >= len(f.buff) {
		return
	}
	f.buff[ind] = color.r
	f.buff[ind+1] = color.g
	f.buff[ind+2] = color.b
}

func (f *MainFrame) getPixel(x, y int) Color {
	ind := f.getInd(x, y)
	return Color{
		r: f.buff[ind],
		g: f.buff[ind+1],
		b: f.buff[ind+2],
	}
}

func init() {
	runtime.LockOSThread()
	appendMode = true
	wallX = 800
	maxY = -1
	minY = 3000
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func (f *MainFrame) draw() {
	for i, el := range points {
		f.drawLine(el, points[(i+1)%len(points)])
	}
}

func (f *MainFrame) drawLine(a, b Point) {
	x := a.x
	y := a.y
	dx := b.x - x
	dy := b.y - y
	if dx > dy && a.x <= b.x && a.y <= b.y {
		e := -dx
		for ; x <= b.x; x++ {
			f.setPixel(x, y, Color{1, 1, 1})
			e += 2 * dy
			if e > 0 {
				e -= 2 * dx
				y += 1
				f.setPixel(x, y, Color{1, 1, 1})
			}
		}
	} else if dx <= dy && a.x <= b.x && a.y <= b.y {
		e := -dy
		for ; y <= b.y; y++ {
			f.setPixel(x, y, Color{1, 1, 1})
			e += 2 * dx
			if e > 0 {
				e -= 2 * dy
				x += 1
				f.setPixel(x, y, Color{1, 1, 1})
			}
		}
	} else if abs(dx) <= abs(dy) && a.x > b.x && a.y <= b.y {
		e := -dy
		for ; y <= b.y; y++ {
			f.setPixel(x, y, Color{1, 1, 1})
			e -= 2 * dx
			if e > 0 {
				e -= 2 * dy
				x -= 1
				f.setPixel(x, y, Color{1, 1, 1})
			}
		}
	} else if abs(dx) > abs(dy) && a.x > b.x && a.y <= b.y {
		e := dx
		for ; x >= b.x; x-- {
			f.setPixel(x, y, Color{1, 1, 1})
			e += 2 * dy
			if e > 0 {
				e += 2 * dx
				y += 1
				f.setPixel(x, y, Color{1, 1, 1})
			}
		}
	} else {
		f.drawLine(b, a)
	}
}

func isExtr(x, y int) bool {
	for i, el := range points {
		if abs(el.x-x) < 4 && abs(el.y-y) < 4 {
			var l, r Point
			if i == 0 {
				l = points[len(points)-1]
			} else {
				l = points[i-1]
			}
			r = points[(i+1)%len(points)]
			return l.y > y && r.y > y || l.y < y && r.y < y
		}
	}
	return false
}

func (f *MainFrame) scanLine() map[int][]int {
	res := make(map[int][]int)
	for y := maxY; y >= minY; y-- {
		var row []int
		cur := -100
		for x := 0; x < f.width; x++ {
			c := f.getPixel(x, y)
			w := Color{1, 1, 1}
			if c == w {
				cur = x
			} else if cur == x-1 {
				row = append(row, cur)
			}
		}
		if cur == f.width-1 {
			row = append(row, cur)
		}
		if len(row)%2 != 0 {
			l := len(row)
			for i := 0; i < l; i++ {
				if isExtr(row[i], y) {
					row = append(row, row[i])
				}
			}
		}
		sort.Ints(row)
		res[y] = row
	}
	return res
}

func (f *MainFrame) fill() {
	intersec := f.scanLine()
	for y := maxY; y >= minY; y-- {
		for i := 0; i < len(intersec[y]); i++ {
			for j := intersec[y][i] + 1; j <= wallX; j++ {
				c := f.getPixel(j, y)
				w := Color{1, 1, 1}
				if c == w {
					f.setPixel(j, y, Color{0, 0, 0})
				} else {
					f.setPixel(j, y, w)
				}
			}
			for j := intersec[y][i]; j > wallX; j-- {
				c := f.getPixel(j, y)
				w := Color{1, 1, 1}
				if c == w {
					f.setPixel(j, y, Color{0, 0, 0})
				} else {
					f.setPixel(j, y, w)
				}
			}
		}
	}
}

func (f *MainFrame) sumColors(i, j int) (sum Color, neighbours int) {
	for k := i - 1; k-i+1 < 3; k++ {
		for m := j - 1; m-j+1 < 3; m++ {
			if k >= 0 && k < f.height && m >= 0 && m < f.width {
				c := f.getPixel(m, k)
				sum.r += c.r
				sum.g += c.g
				sum.b += c.b
				neighbours++
			}
		}
	}
	c := f.getPixel(j, i)
	sum.r -= c.r
	sum.g -= c.g
	sum.b -= c.b
	neighbours--
	return
}

func (f *MainFrame) postFiltrate() {
	tmpBuf := make([]float32, len(f.buff))
	for i := 0; i < f.height; i++ {
		for j := 0; j < f.width; j++ {
			c, neighbours := f.sumColors(i, j)
			cc := f.getPixel(j, i)
			ind := f.getInd(j, i)
			tmpBuf[ind] = c.r/float32(neighbours)/2 + cc.r/2
			tmpBuf[ind+1] = c.g/float32(neighbours)/2 + cc.g/2
			tmpBuf[ind+2] = c.b/float32(neighbours)/2 + cc.b/2
		}
	}
	f.buff = tmpBuf
}

func (f *MainFrame) drawWall() {
	for y := 0; y < f.height; y++ {
		f.setPixel(wallX, y, Color{1, 0, 0})
	}
}

func show(window *glfw.Window) {
	w, h := window.GetSize()
	f := initFrame(w, h)
	f.draw()
	if fillMode {
		f.fill()
	}
	if filterMode {
		f.postFiltrate()
	}
	f.drawWall()
	gl.DrawPixels(int32(w), int32(h), gl.RGB, gl.FLOAT, unsafe.Pointer(&f.buff[0]))
}

func mouseButtonCallback(w *glfw.Window, b glfw.MouseButton, action glfw.Action, _ glfw.ModifierKey) {
	_, h := w.GetSize()
	x1, y1 := w.GetCursorPos()
	x := int(x1)
	y := h - int(y1) - 1
	if action == glfw.Release {
		switch b {
		case glfw.MouseButton1:
			if appendMode {
				if y > maxY {
					maxY = y
				}
				if y < minY {
					minY = y
				}
				points = append(points, Point{x, y})
			} else if wall {
				wallX = x
				fmt.Println(wallX)
			}
		}
	}
}

func keyCallback(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
	if key == glfw.KeyEscape {
		w.SetShouldClose(true)
	} else if action == glfw.Release {
		switch key {
		case glfw.KeyP:
			appendMode = false
			fillMode = false
			wall = true
		case glfw.KeyF:
			wall = false
			appendMode = false
			fillMode = !fillMode
		case glfw.KeyR:
			wall = false
			fillMode = false
			appendMode = true
			points = []Point{}
		case glfw.KeySpace:
			wall = false
			appendMode = false
			filterMode = !filterMode
		}
	}
}

func resizeCallback(w *glfw.Window) {
	points = []Point{}
	appendMode = true
	wall = false
	wallX = 0
	fillMode = false
	filterMode = false
	maxY = -1
	minY = 3000
}

func main() {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(width, height, "Lab4", nil, nil)
	if err != nil {
		log.Fatal(err.Error())
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatal(err.Error())
	}

	window.SetMouseButtonCallback(mouseButtonCallback)
	window.SetKeyCallback(keyCallback)
	window.SetRefreshCallback(resizeCallback)

	for !window.ShouldClose() {
		glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT)
		show(window)
		window.SwapBuffers()
	}
}
