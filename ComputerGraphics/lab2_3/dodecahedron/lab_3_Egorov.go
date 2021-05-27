package main

import (
	"fmt"
	"log"
	"math"
	"math/rand"
	"runtime"
	"time"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

const (
	width     = 800
	height    = 800
	angleMove = 5
	posDelta  = 0.1
)

type Point struct {
	x float64
	y float64
	z float64
}

type Color struct {
	r float64
	g float64
	b float64
}

type Figure struct {
	colors      []Color
	startPoint  Point
	angle       Point
	scale       float64
	polygonMode int
	projections [][]float64
	viewPorts   [][]int32
}

type Dodecahedron struct {
	figure Figure
	points [][]*Point
	colors []Color
}

type Cube struct {
	figure  Figure
	points  []Point
	borders [][]int
}

func (p *Point) drawPoint() {
	gl.Vertex3d(p.x, p.y, p.z)
}

func (c *Color) setColor() {
	gl.Color3d(c.r, c.g, c.b)
}

func (f *Figure) init() {
	f.viewPorts = [][]int32{
		{0, height / 2, width / 2, height / 2},
		{width / 2, height / 2, width / 2, height / 2},
		{width / 2, 0, width / 2, height / 2},
	}

	f.projections = [][]float64{
		{ //Z
			1, 0, 0, 0,
			0, 1, 0, 0,
			0, 0, -1, 0,
			0, 0, 0, 1,
		},
		{ //X
			0, 0, -1, 0,
			0, 1, 0, 0,
			-1, 0, 0, 0,
			0, 0, 0, 1,
		},
		{ //Y
			1, 0, 0, 0,
			0, 0, -1, 0,
			0, -1, 0, 0,
			0, 0, 0, 1,
		},
	}
}

func (f *Figure) configurate() {
	gl.Ortho(-1, 1, -1, 1, -1, 1)
	gl.PolygonMode(gl.FRONT_AND_BACK, uint32(f.polygonMode))
	gl.Translated(f.startPoint.x, f.startPoint.y, f.startPoint.z)
	gl.Scaled(f.scale, f.scale, f.scale)
	gl.Rotated(f.angle.x, 1, 0, 0)
	gl.Rotated(f.angle.y, 0, 1, 0)
	gl.Rotated(f.angle.z, 0, 0, 1)
}

func (f *Figure) setProjection(indProjection int) {
	if indProjection >= len(f.projections) || indProjection >= len(f.viewPorts) {
		return
	}
	gl.Viewport(f.viewPorts[indProjection][0],
		f.viewPorts[indProjection][1],
		f.viewPorts[indProjection][2],
		f.viewPorts[indProjection][3])
	gl.MatrixMode(gl.PROJECTION)
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.LoadIdentity()
	gl.MultMatrixd(&f.projections[indProjection][0])

	gl.MatrixMode(gl.MODELVIEW)
	gl.LoadIdentity()
}

func (dodecahedron *Dodecahedron) getPoint(h, w int) *Point {
	for len(dodecahedron.points) <= h {
		dodecahedron.points = append(dodecahedron.points, []*Point{})
	}
	for len(dodecahedron.points[h]) <= w {
		dodecahedron.points[h] = append(dodecahedron.points[h], nil)
	}
	if dodecahedron.points[h][w] == nil {
		log.Println(h, w)
		var teta, phi float64
		if h == 0 || h == 3 {
			teta = math.Asin(2 / (math.Sqrt(3) * (1 + math.Sqrt(5)) * math.Sin(math.Pi/5)))
		} else {
			teta = math.Asin(1 / (math.Sqrt(3) * math.Sin(math.Pi/5)))
		}
		if h >= 2 {
			teta = math.Pi - teta
		}
		phi = 2 * math.Pi * float64(2*w+h/2) / float64(10)
		dodecahedron.points[h][w] = &Point{
			x: math.Sin(teta) * math.Cos(phi),
			y: math.Sin(teta) * math.Sin(phi),
			z: math.Cos(teta),
		}
	}
	return dodecahedron.points[h][w]
}

func (dodecahedron *Dodecahedron) draw() {
	for i := 0; i < 3; i++ {
		dodecahedron.figure.setProjection(i)
		gl.PushMatrix()
		defer gl.PopMatrix()
		dodecahedron.figure.configurate()

		gl.Begin(gl.POLYGON)
		dodecahedron.colors[0].setColor()
		for i := 0; i < 5; i++ {
			dodecahedron.getPoint(0, i%5).drawPoint()
		}
		gl.End()

		for i := 0; i < 5; i++ {
			gl.Begin(gl.POLYGON)
			dodecahedron.colors[(i+1)%len(dodecahedron.colors)].setColor()

			dodecahedron.getPoint(0, i).drawPoint()
			dodecahedron.getPoint(0, (i+1)%5).drawPoint()
			dodecahedron.getPoint(1, (i+1)%5).drawPoint()
			dodecahedron.getPoint(2, i).drawPoint()
			dodecahedron.getPoint(1, i).drawPoint()

			gl.End()
		}

		for i := 0; i < 5; i++ {
			gl.Begin(gl.POLYGON)
			dodecahedron.colors[(i+6)%len(dodecahedron.colors)].setColor()

			dodecahedron.getPoint(3, (i+1)%5).drawPoint()
			dodecahedron.getPoint(3, i).drawPoint()
			dodecahedron.getPoint(2, i).drawPoint()
			dodecahedron.getPoint(1, (i+1)%5).drawPoint()
			dodecahedron.getPoint(2, (i+1)%5).drawPoint()

			gl.End()
		}

		gl.Begin(gl.POLYGON)
		dodecahedron.colors[11%len(dodecahedron.colors)].setColor()
		for i := 0; i <= 5; i++ {
			dodecahedron.getPoint(3, i%5).drawPoint()
		}
		gl.End()
	}
}

func (dodecahedron *Dodecahedron) init() {
	dodecahedron.figure.init()
	for i := 0; i < 14; i++ {
		dodecahedron.colors = append(dodecahedron.colors, Color{rand.Float64(), rand.Float64(), rand.Float64()})
	}
}

func (c *Cube) draw() {
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.Viewport(0, 0, width/2, height/2)

	gl.MatrixMode(gl.MODELVIEW)
	gl.LoadIdentity()
	c.figure.configurate()
	gl.Begin(gl.QUADS)
	for i, ind := range c.borders {
		gl.Color3d(c.figure.colors[i%len(c.figure.colors)].r,
			c.figure.colors[i%len(c.figure.colors)].g,
			c.figure.colors[i%len(c.figure.colors)].b)
		for _, v := range ind {
			gl.Vertex3d(c.points[v].x, c.points[v].y, c.points[v].z)
		}
	}
	gl.End()
}

func (c *Cube) init() {
	c.points = []Point{
		{1, -1, 1},   // 0
		{1, 1, 1},    // 1
		{-1, 1, 1},   // 2
		{-1, -1, 1},  // 3
		{1, -1, -1},  // 4
		{1, 1, -1},   // 5
		{-1, 1, -1},  // 6
		{-1, -1, -1}, // 7
	}
	c.borders = [][]int{
		{0, 1, 2, 3},
		{0, 1, 5, 4},
		{1, 2, 6, 5},
		{3, 0, 4, 7},
		{2, 3, 7, 6},
		{4, 5, 6, 7},
	}
}

func (f *Figure) KeyCallback(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
	if key == glfw.KeyEscape {
		w.SetShouldClose(true)
	} else if action == glfw.Press || action == glfw.Repeat {
		switch key {
		case glfw.KeyA:
			f.angle.y -= angleMove
		case glfw.KeyD:
			f.angle.y += angleMove
		case glfw.KeyS:
			f.angle.x -= angleMove
		case glfw.KeyW:
			f.angle.x += angleMove
		case glfw.KeyQ:
			f.angle.z += angleMove
		case glfw.KeyE:
			f.angle.z -= angleMove
		case glfw.KeyEqual:
			if f.scale < 0.5 {
				f.scale += 0.01
			}
		case glfw.KeyMinus:
			if f.scale > 0 {
				f.scale -= 0.01
			}
		case glfw.KeyUp:
			f.startPoint.y += posDelta
		case glfw.KeyDown:
			f.startPoint.y -= posDelta
		case glfw.KeyRight:
			f.startPoint.x += posDelta
		case glfw.KeyLeft:
			f.startPoint.x -= posDelta
		case glfw.KeySpace:
			if f.polygonMode == gl.FILL {
				f.polygonMode = gl.LINE
			} else {
				f.polygonMode = gl.FILL
			}
		default:
			log.Println(key, action)
		}
	}
}

func init() {
	rand.Seed(time.Now().UnixNano())
	runtime.LockOSThread()
}

func main() {
	fmt.Println("HI")
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, glfw.False)
	window, err := glfw.CreateWindow(width, height, "Lab2", nil, nil)
	if err != nil {
		log.Fatal(err.Error())
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatal(err.Error())
	}

	dodecahedron := Dodecahedron{
		figure: Figure{
			startPoint:  Point{0, 0, 0},
			scale:       0.5,
			polygonMode: gl.FILL,
		},
		// colors: []Color{
		// 	{1, 0, 0},
		// 	{1, 0.64, 0},
		// 	{1, 1, 0},
		// 	{0, 1, 0},
		// 	{0, 1, 1},
		// 	{0, 0, 1},
		// 	{1, 0, 1},
		// },
	}

	dodecahedron.init()

	example := Cube{
		figure: Figure{
			colors: []Color{
				{1, 0, 0},
				{0, 1, 0},
				{0, 0, 1},
				{1, 1, 0},
				{0, 1, 1},
				{1, 0, 1},
			},
			startPoint:  Point{0, 0, 0},
			angle:       Point{45, 45, 0},
			scale:       0.2,
			polygonMode: gl.LINE,
		},
	}

	example.init()

	window.SetKeyCallback(glfw.KeyCallback(dodecahedron.figure.KeyCallback))

	gl.Enable(gl.DEPTH_TEST)

	for !window.ShouldClose() {
		glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
		dodecahedron.draw()
		example.draw()
		window.SwapBuffers()

	}
}
