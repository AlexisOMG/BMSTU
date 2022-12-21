package utils

import (
	"fmt"
	"image"
	"os"

	_ "golang.org/x/image/bmp"
)

type Sample struct {
	Path     string
	Input    Vector
	Expected Vector
}

func GetSamples(pathToDir string) ([]Sample, error) {
	var samples []Sample

	for i := 0; i < 10; i++ {
		res := NewEmptyVector(10)
		res.Values[i] = 1

		for j := 0; j < 10; j++ {
			file, err := os.Open(fmt.Sprintf("%s/%d_%d.bmp", pathToDir, i, j))
			if err != nil {
				return nil, err
			}

			img, _, err := image.Decode(file)
			if err != nil {
				file.Close()
				return nil, err
			}

			vec := NewVector([]float64{})

			grayIMG := image.NewGray(img.Bounds())

			for k := 0; k < img.Bounds().Dx(); k++ {
				for l := 0; l < img.Bounds().Dy(); l++ {
					gray, _, _, _ := grayIMG.ColorModel().Convert(img.At(k, l)).RGBA()
					vec.Values = append(vec.Values, float64(gray))
				}
			}

			samples = append(samples, Sample{
				Path:     fmt.Sprintf("%s/%d_%d.bmp", pathToDir, i, j),
				Input:    vec.MultiplyScalar(1.0 / vec.EuclideanLength()),
				Expected: res,
			})

			file.Close()
		}
	}

	return samples, nil
}
