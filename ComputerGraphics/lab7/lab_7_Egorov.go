package main

import (
	"encoding/json"
	"image"
	"image/draw"
	_ "image/png"
	"io"
	"io/ioutil"
	"log"
	"math"
	"math/rand"
	"os"
	"runtime"
	"time"
	"unsafe"

	"github.com/go-gl/gl/v2.1/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

const (
	width     = 300
	height    = 300
	angleMove = 5
	posDelta  = 0.1
)

var (
	texture      uint32
	infLight     bool
	st           State
	dodecahedron Dodecahedron
	t            float32 = 0.0
	polygons     [12][]float64
	normals      [12][]float64
	texCoords    [12][]float64
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
	Colors      []Color     `json:"colors,omitempty"`
	StartPoint  Point       `json:"start_point,omitempty"`
	Angle       Point       `json:"angle,omitempty"`
	Scale       float64     `json:"scale,omitempty"`
	PolygonMode int         `json:"polygon_mode,omitempty"`
	Projections [][]float64 `json:"projections,omitempty"`
	ViewPorts   [][]int32   `json:"view_ports,omitempty"`
}

type Dodecahedron struct {
	Figure Figure     `json:"figure,omitempty"`
	Points [][]*Point `json:"points,omitempty"`
	Colors []Color    `json:"colors,omitempty"`
}

type State struct {
	Dodik    Dodecahedron `json:"dodik,omitempty"`
	InfLight bool         `json:"infLight,omitempty"`
	T        float32      `json:"param,omitempty"`
}

func (p *Point) drawPoint() {
	gl.Vertex3d(p.x, p.y, p.z)
}

func (c *Color) setColor() {
	gl.Color3d(c.r, c.g, c.b)
}

func (f *Figure) init() {
	f.ViewPorts = [][]int32{
		{0, height / 2, width / 2, height / 2},
		{width / 2, height / 2, width / 2, height / 2},
		{width / 2, 0, width / 2, height / 2},
	}

	f.Projections = [][]float64{
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
	gl.PolygonMode(gl.FRONT_AND_BACK, uint32(f.PolygonMode))
	gl.Translated(f.StartPoint.x, f.StartPoint.y, f.StartPoint.z)
	gl.Scaled(f.Scale, f.Scale, f.Scale)
	gl.Rotated(f.Angle.x, 1, 0, 0)
	gl.Rotated(f.Angle.y, 0, 1, 0)
	gl.Rotated(f.Angle.z, 0, 0, 1)
}

func (f *Figure) setProjection(indProjection int) {
	if indProjection >= len(f.Projections) || indProjection >= len(f.ViewPorts) {
		return
	}
	gl.Viewport(f.ViewPorts[indProjection][0],
		f.ViewPorts[indProjection][1],
		f.ViewPorts[indProjection][2],
		f.ViewPorts[indProjection][3])
	gl.MatrixMode(gl.PROJECTION)
	gl.PushMatrix()
	defer gl.PopMatrix()
	gl.LoadIdentity()
	gl.MultMatrixd(&f.Projections[indProjection][0])

	gl.MatrixMode(gl.MODELVIEW)
	gl.LoadIdentity()
}

func (dodecahedron *Dodecahedron) getPoint(h, w int) *Point {
	for len(dodecahedron.Points) <= h {
		dodecahedron.Points = append(dodecahedron.Points, []*Point{})
	}
	for len(dodecahedron.Points[h]) <= w {
		dodecahedron.Points[h] = append(dodecahedron.Points[h], nil)
	}
	if dodecahedron.Points[h][w] == nil {
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
		dodecahedron.Points[h][w] = &Point{
			x: math.Sin(teta) * math.Cos(phi),
			y: math.Sin(teta) * math.Sin(phi),
			z: math.Cos(teta),
		}
	}
	return dodecahedron.Points[h][w]
}

func getN(a, b, c Point) Point {
	v1 := Point{
		x: c.x - a.x,
		y: c.y - a.y,
		z: c.z - a.z,
	}
	v2 := Point{
		x: b.x - a.x,
		y: b.y - a.y,
		z: b.z - a.z,
	}

	answ := Point{
		x: v1.y*v2.z - v1.z*v2.y,
		y: v1.z*v2.x - v1.x*v2.z,
		z: v1.x*v2.y - v1.y*v2.x,
	}

	length := math.Sqrt(answ.x*answ.x + answ.y*answ.y + answ.z*answ.z)

	answ.x /= length
	answ.y /= length
	answ.z /= length

	return answ
}

func setLight(l Point) {
	var diffuse []float32 = []float32{0.4, 0.7, 0.2}
	var posit []float32 = []float32{0, 0, 1, 1}
	var ambi []float32 = []float32{1, 1, 1, 1}
	posit[0] = float32(l.x)
	posit[1] = float32(l.y)
	posit[2] = float32(l.z)
	if infLight {
		posit[3] = 0
	} else {
		posit[3] = 1
	}
	var cAtten, lAtten, qAtten float32 = 0, 0.2, 0.4
	gl.Lightfv(gl.LIGHT0, gl.DIFFUSE, &diffuse[0])
	gl.Lightfv(gl.LIGHT0, gl.POSITION, &posit[0])
	gl.Lightfv(gl.LIGHT0, gl.AMBIENT, &ambi[0])
	gl.Lightfv(gl.LIGHT0, gl.CONSTANT_ATTENUATION, &cAtten)
	gl.Lightfv(gl.LIGHT0, gl.LINEAR_ATTENUATION, &lAtten)
	gl.Lightfv(gl.LIGHT0, gl.QUADRATIC_ATTENUATION, &qAtten)
	gl.ShadeModel(gl.FLAT)
}

func appendNormals(x, y, z float64, i int) {
	normals[i] = append(normals[i], x, y, z)
}

func appendTex(x, y float64, i int) {
	texCoords[i] = append(texCoords[i], x, y)
}

func appendPolygon(a Point, i int) {
	polygons[i] = append(polygons[i], a.x, a.y, a.z)
}

func render() {
	for i := 0; i < 12; i++ {
		gl.NormalPointer(gl.DOUBLE, 0, unsafe.Pointer(&normals[i][0]))

		gl.VertexPointer(3, gl.DOUBLE, 0, unsafe.Pointer(&polygons[i][0]))

		gl.TexCoordPointer(2, gl.DOUBLE, 0, unsafe.Pointer(&texCoords[i][0]))

		gl.DrawArrays(gl.POLYGON, 0, 5)
	}
}

func clearArr() {
	polygons = [12][]float64{}
	normals = [12][]float64{}
	texCoords = [12][]float64{}
}

func (dodecahedron *Dodecahedron) draw(light Point) {
	for i := 0; i < 3; i++ {
		dodecahedron.Figure.setProjection(i)
		setLight(light)
		gl.PushMatrix()
		defer gl.PopMatrix()
		dodecahedron.Figure.configurate()

		n1 := getN(*dodecahedron.getPoint(0, 0), *dodecahedron.getPoint(0, 1), *dodecahedron.getPoint(0, 2))

		clearArr()
		ind := 0

		for i := 0; i < 5; i++ {
			appendNormals(-n1.x, -n1.y, -n1.z, ind)
			appendTex(float64(i)/5.0, float64(i)/5.0, ind)
			appendPolygon(*dodecahedron.getPoint(0, i%5), ind)
		}

		ind++

		for i := 0; i < 5; i++ {

			a := dodecahedron.getPoint(0, i)
			b := dodecahedron.getPoint(0, (i+1)%5)
			c := dodecahedron.getPoint(1, (i+1)%5)
			d := dodecahedron.getPoint(2, i)
			e := dodecahedron.getPoint(1, i)

			n := getN(*a, *b, *c)

			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)

			appendTex(0, float64(i)/5.0, ind)
			appendPolygon(*a, ind)

			appendTex(0, float64((i+1)%5)/5.0, ind)
			appendPolygon(*b, ind)

			appendTex(0.2, float64((i+1)%5)/5.0, ind)
			appendPolygon(*c, ind)

			appendTex(0.4, float64(i)/5.0, ind)
			appendPolygon(*d, ind)

			appendTex(0.2, float64(i)/5.0, ind)
			appendPolygon(*e, ind)

			ind++
		}

		for i := 0; i < 5; i++ {

			a := dodecahedron.getPoint(3, (i+1)%5)
			b := dodecahedron.getPoint(3, i)
			c := dodecahedron.getPoint(2, i)
			d := dodecahedron.getPoint(1, (i+1)%5)
			e := dodecahedron.getPoint(2, (i+1)%5)

			n := getN(*a, *b, *c)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)
			appendNormals(n.x, n.y, n.z, ind)

			appendTex(0.6, float64((i+1)%5)/5.0, ind)
			appendPolygon(*a, ind)

			appendTex(0.6, float64(i)/5.0, ind)
			appendPolygon(*b, ind)

			appendTex(0.4, float64(i)/5.0, ind)
			appendPolygon(*c, ind)

			appendTex(0.2, float64((i+1)%5)/5.0, ind)
			appendPolygon(*d, ind)

			appendTex(0.4, float64((i+1)%5)/5.0, ind)
			appendPolygon(*e, ind)

			ind++
		}

		n1 = getN(*dodecahedron.getPoint(3, 0), *dodecahedron.getPoint(3, 1), *dodecahedron.getPoint(3, 2))

		for i := 0; i < 5; i++ {
			appendNormals(n1.x, n1.y, n1.z, ind)
			appendTex(float64(i)/5.0, float64(i)/5.0, ind)
			appendPolygon(*dodecahedron.getPoint(3, i%5), ind)
		}
		render()

	}
}

func (dodecahedron *Dodecahedron) init() {
	dodecahedron.Figure.init()
	for i := 0; i < 14; i++ {
		dodecahedron.Colors = append(dodecahedron.Colors, Color{rand.Float64(), rand.Float64(), rand.Float64()})
	}
}

func (f *Figure) KeyCallback(w *glfw.Window, key glfw.Key, scancode int, action glfw.Action, mods glfw.ModifierKey) {
	if key == glfw.KeyEscape {
		w.SetShouldClose(true)
	} else if action == glfw.Press || action == glfw.Repeat {
		switch key {
		case glfw.KeyM:
			st.Dodik = dodecahedron
			st.InfLight = infLight
			st.T = t
			file, err := os.Create("state.json")
			if err != nil {
				panic(err.Error())
			}
			s, err := json.Marshal(st)
			if err != nil {
				panic(err.Error())
			}
			io.WriteString(file, string(s))
		case glfw.KeyN:
			b, err := ioutil.ReadFile("state.json")
			if err != nil {
				panic(err.Error())
			}
			err = json.Unmarshal(b, &st)
			if err != nil {
				panic(err.Error())
			}
			dodecahedron = st.Dodik
			t = st.T
			infLight = st.InfLight
		case glfw.KeyI:
			infLight = !infLight
		case glfw.KeyA:
			f.Angle.y -= angleMove
		case glfw.KeyD:
			f.Angle.y += angleMove
		case glfw.KeyS:
			f.Angle.x -= angleMove
		case glfw.KeyW:
			f.Angle.x += angleMove
		case glfw.KeyQ:
			f.Angle.z += angleMove
		case glfw.KeyE:
			f.Angle.z -= angleMove
		case glfw.KeyEqual:
			if f.Scale < 0.5 {
				f.Scale += 0.01
			}
		case glfw.KeyMinus:
			if f.Scale > 0 {
				f.Scale -= 0.01
			}
		case glfw.KeyUp:
			f.StartPoint.y += posDelta
		case glfw.KeyDown:
			f.StartPoint.y -= posDelta
		case glfw.KeyRight:
			f.StartPoint.x += posDelta
		case glfw.KeyLeft:
			f.StartPoint.x -= posDelta
		case glfw.KeySpace:
			if f.PolygonMode == gl.FILL {
				f.PolygonMode = gl.LINE
			} else {
				f.PolygonMode = gl.FILL
			}
		default:
			log.Println(key, action)
		}
	}
}

func twin(t float32) Point {
	p0 := Point{0.85, 0.5, 0.21}
	p1 := Point{0.59, 0.5, 1.92}
	p2 := Point{-2.07, 0.5, -0.45}
	p3 := Point{-0.19, 0.5, -0.905}
	return Point{
		x: math.Pow(float64(1-t), 3)*p0.x + 3*float64((1-t)*(1-t)*t)*p1.x + 3*float64(t*t*(1-t))*p2.x + math.Pow(float64(t), 3)*p3.x,
		y: math.Pow(float64(1-t), 3)*p0.y + 3*float64((1-t)*(1-t)*t)*p1.y + 3*float64(t*t*(1-t))*p2.y + math.Pow(float64(t), 3)*p3.y,
		z: math.Pow(float64(1-t), 3)*p0.z + 3*float64((1-t)*(1-t)*t)*p1.z + 3*float64(t*t*(1-t))*p2.z + math.Pow(float64(t), 3)*p3.z,
	}
}

func newTexture(file string) uint32 {
	imgFile, err := os.Open(file)
	if err != nil {
		log.Fatalf("texture %q not found on disk: %v\n", file, err)
	}
	img, _, err := image.Decode(imgFile)
	if err != nil {
		panic(err)
	}

	rgba := image.NewRGBA(img.Bounds())
	if rgba.Stride != rgba.Rect.Size().X*4 {
		panic("unsupported stride")
	}
	draw.Draw(rgba, rgba.Bounds(), img, image.Point{0, 0}, draw.Src)

	var texture uint32
	gl.Enable(gl.TEXTURE_2D)
	gl.GenTextures(1, &texture)
	gl.BindTexture(gl.TEXTURE_2D, texture)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT)
	gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT)
	gl.TexImage2D(
		gl.TEXTURE_2D,
		0,
		gl.RGBA,
		int32(rgba.Rect.Size().X),
		int32(rgba.Rect.Size().Y),
		0,
		gl.RGBA,
		gl.UNSIGNED_BYTE,
		gl.Ptr(rgba.Pix))

	return texture
}

func init() {
	rand.Seed(time.Now().UnixNano())
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(); err != nil {
		log.Fatalln("failed to initialize glfw:", err)
	}
	defer glfw.Terminate()

	glfw.WindowHint(glfw.Resizable, glfw.False)
	window, err := glfw.CreateWindow(width, height, "Lab7", nil, nil)
	if err != nil {
		log.Fatal("wtf", err.Error())
	}
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		log.Fatal("Unable to gl.Init", err.Error())
	}

	dodecahedron = Dodecahedron{
		Figure: Figure{
			StartPoint:  Point{0, 0, 0},
			Scale:       0.5,
			PolygonMode: gl.FILL,
		},
	}

	dodecahedron.init()

	window.SetKeyCallback(glfw.KeyCallback(dodecahedron.Figure.KeyCallback))

	texture = newTexture("grad.png")
	defer gl.DeleteTextures(1, &texture)
	gl.ClearDepth(1)
	gl.DepthFunc(gl.LEQUAL)
	// gl.TexEnvi(gl.TEXTURE_ENV, gl.TEXTURE_ENV_MODE, gl.MODULATE)

	gl.Enable(gl.DEPTH_TEST)
	gl.LoadIdentity()
	gl.Enable(gl.LIGHTING)
	gl.Enable(gl.LIGHT0)
	// gl.Enable(gl.NORMALIZE)
	gl.ShadeModel(gl.FLAT)

	gl.EnableClientState(gl.VERTEX_ARRAY)
	gl.EnableClientState(gl.NORMAL_ARRAY)
	gl.EnableClientState(gl.TEXTURE_COORD_ARRAY)

	for !window.ShouldClose() {

		l := twin(t)

		t = (t + 0.001)
		t -= float32(math.Floor(float64(t)))

		glfw.PollEvents()

		gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

		tm := time.Now()
		dodecahedron.draw(l)
		dt := time.Since(tm).Microseconds()
		println(dt)

		window.SwapBuffers()
	}

	gl.Disable(gl.LIGHT0)
}
