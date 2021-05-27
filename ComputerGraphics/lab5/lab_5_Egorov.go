package main

import (
	"fmt"
	"log"
	"runtime"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

var width, height int = 800, 800
var lines []Vector
var polygon []Side
var cutMode bool
var angleX, angleY, angleZ float64
var angleMove float64 = 3

func dotProduct(a, b Point) float64 {
	return a.x*b.x + a.y*b.y + a.z*b.z
}

type Point struct {
	x float64
	y float64
	z float64
}

type Vector struct {
	p0 Point
	p1 Point

	isDisplayed bool
}

func (v *Vector) getVector() Point {
	return Point{
		x: v.p1.x - v.p0.x,
		y: v.p1.y - v.p0.y,
		z: v.p1.z - v.p0.z,
	}
}

func (v *Vector) display() {
	if v.isDisplayed {
		gl.Color3d(0.8, 0, 0)
		gl.Vertex3d(v.p0.x, v.p0.y, v.p0.z)
		gl.Vertex3d(v.p1.x, v.p1.y, v.p1.z)
	}
}

type Side struct {
	A Point
	B Point
	C Point
	D Point

	n Point
}

func (s *Side) display() {
	gl.Color3d(1, 1, 1)
	gl.Vertex3d(s.A.x, s.A.y, s.A.z)
	gl.Vertex3d(s.B.x, s.B.y, s.B.z)

	gl.Vertex3d(s.B.x, s.B.y, s.B.z)
	gl.Vertex3d(s.C.x, s.C.y, s.C.z)

	gl.Vertex3d(s.C.x, s.C.y, s.C.z)
	gl.Vertex3d(s.D.x, s.D.y, s.D.z)

	gl.Vertex3d(s.D.x, s.D.y, s.D.z)
	gl.Vertex3d(s.A.x, s.A.y, s.A.z)
}

func max(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

func min(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}

func CyrusBack() {
	for i, line := range lines {
		var Q, P float64
		var tin, tout float64 = 0.0, 1.0
		D := line.getVector()
		for _, side := range polygon {
			W := Point{
				x: side.A.x - line.p0.x,
				y: side.A.y - line.p0.y,
				z: side.A.z - line.p0.z,
			}
			Q = dotProduct(side.n, W)
			P = dotProduct(side.n, D)
			if P == 0 {
				if Q >= 0 {
					line.isDisplayed = true
					break
				}
			} else if P > 0 {
				t := Q / P
				if t >= 0 && t <= 1 {
					tin = max(tin, t)
					continue
				}
			} else if P < 0 {
				t := Q / P
				if t >= 0 && t <= 1 {
					tout = min(tout, t)
					continue
				}
			}
		}
		lines[i].p1.x = line.p0.x + D.x*tout
		lines[i].p1.y = line.p0.y + D.y*tout
		lines[i].p1.z = line.p0.z + D.z*tout

		lines[i].p0.x = line.p0.x + D.x*tin
		lines[i].p0.y = line.p0.y + D.y*tin
		lines[i].p0.z = line.p0.z + D.z*tin
		line.display()
	}
}

func draw() {
	for _, side := range polygon {
		side.display()
	}
	if cutMode {
		CyrusBack()
	}
	if !cutMode {
		for _, line := range lines {
			line.display()
		}
	}
}

func keyCallback(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
	if key == glfw.KeyEscape {
		w.SetShouldClose(true)
	} else if action == glfw.Press || action == glfw.Repeat {
		switch key {
		case glfw.KeyF:
			cutMode = !cutMode
			if !cutMode {
				for i := range lines {
					lines[i].isDisplayed = true
				}
			}
			// fmt.Println(lines)
		case glfw.KeyA:
			angleY -= angleMove
		case glfw.KeyD:
			angleY += angleMove
		case glfw.KeyS:
			angleX -= angleMove
		case glfw.KeyW:
			angleX += angleMove
		case glfw.KeyQ:
			angleZ += angleMove
		case glfw.KeyE:
			angleZ -= angleMove
		case glfw.KeyI:
			readLine()
		}
	}
}

func readLine() {
	fmt.Println("Input")
	var p0, p1 Point

	fmt.Scanf("%f%f%f%f%f%f", &p0.x, &p0.y, &p0.z, &p1.x, &p1.y, &p1.z)

	lines = append(lines, Vector{p0: p0, p1: p1, isDisplayed: true})
}

func init() {
	runtime.LockOSThread()
	polygon = append(polygon, Side{
		A: Point{0.3, 0, 0},
		B: Point{0.3, 0, 0.3},
		C: Point{0, 0, 0.3},
		D: Point{0.0, 0, 0},
		n: Point{0.0, 1, 0},
	})
	polygon = append(polygon, Side{
		A: Point{0.0, 0, 0},
		B: Point{0, 0, 0.3},
		C: Point{0, 0.3, 0.3},
		D: Point{0, 0.3, 0},
		n: Point{1, 0, 0},
	})
	polygon = append(polygon, Side{
		A: Point{0, 0.3, 0},
		B: Point{0, 0.3, 0.3},
		C: Point{0.3, 0.3, 0.3},
		D: Point{0.3, 0.3, 0},
		n: Point{0, -1, 0.0},
	})
	polygon = append(polygon, Side{
		A: Point{0.3, 0.3, 0},
		B: Point{0.3, 0.3, 0.3},
		C: Point{0.3, 0, 0.3},
		D: Point{0.3, 0, 0},
		n: Point{-1, 0.0, 0.0},
	})
	polygon = append(polygon, Side{
		A: Point{0, 0, 0},
		B: Point{0, 0.3, 0},
		C: Point{0.3, 0.3, 0},
		D: Point{0.3, 0, 0},
		n: Point{0.0, 0, 1},
	})
	polygon = append(polygon, Side{
		A: Point{0, 0, 0.3},
		B: Point{0, 0.3, 0.3},
		C: Point{0.3, 0.3, 0.3},
		D: Point{0.3, 0, 0.3},
		n: Point{0.0, 0, -1},
	})
}

func main() {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, glfw.False)

	window, err := glfw.CreateWindow(width, height, "Lab5", nil, nil)
	if err != nil {
		log.Fatal(err.Error())
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatal(err.Error())
	}

	gl.Enable(gl.DEPTH_TEST)
	window.SetKeyCallback(glfw.KeyCallback(keyCallback))

	gl.Viewport(0, 0, 800, 800)

	gl.MatrixMode(gl.MODELVIEW)
	// gl.PolygonMode(gl.FRONT_AND_BACK, gl.LINE)
	gl.LoadIdentity()

	for !window.ShouldClose() {

		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		gl.PushMatrix()

		gl.Rotated(angleX, 1, 0, 0)
		gl.Rotated(angleY, 0, 1, 0)
		gl.Rotated(angleZ, 0, 0, 1)

		gl.Begin(gl.LINES)
		draw()
		gl.End()

		gl.PopMatrix()

		window.SwapBuffers()

		glfw.PollEvents()
	}
}
