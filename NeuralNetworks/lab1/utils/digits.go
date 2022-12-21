package utils

import (
	"fmt"
	"image"
	"image/color"
	"math/rand"
	"os"

	"golang.org/x/image/bmp"
)

func GetSample(pathToFile string) (Sample, error) {
	file, err := os.Open(pathToFile)
	if err != nil {
		return Sample{}, err
	}
	defer file.Close()

	img, _, err := image.Decode(file)
	if err != nil {
		return Sample{}, err
	}

	vec := NewVector([]float64{})

	grayIMG := image.NewGray(img.Bounds())

	for k := 0; k < img.Bounds().Dx(); k++ {
		for l := 0; l < img.Bounds().Dy(); l++ {
			gray, _, _, _ := grayIMG.ColorModel().Convert(img.At(k, l)).RGBA()
			vec.Values = append(vec.Values, float64(gray))
		}
	}

	return Sample{
		Input: vec.MultiplyScalar(1.0 / vec.EuclideanLength()),
	}, nil
}

func CreatePictures(pathToExamples, pathToOutput string) error {
	for i := 0; i < 10; i++ {
		file, err := os.Open(fmt.Sprintf("%s/%d.bmp", pathToExamples, i))
		if err != nil {
			return err
		}

		img, _, err := image.Decode(file)
		if err != nil {
			file.Close()
			return err
		}

		for j := 0; j < 10; j++ {
			nimg := image.NewGray(img.Bounds())

			for k := 0; k < img.Bounds().Dx(); k++ {
				for l := 0; l < img.Bounds().Dy(); l++ {
					color := nimg.ColorModel().Convert(img.At(k, l))
					nimg.Set(k, l, color)
				}
			}

			x, y := rand.Intn(nimg.Bounds().Dx()-1), rand.Intn(nimg.Bounds().Dy()-1)

			nimg.SetGray(x, y, color.Gray{150})

			out, err := os.Create(fmt.Sprintf("%s/%d_%d.bmp", pathToOutput, i, j))
			if err != nil {
				file.Close()
				return err
			}

			err = bmp.Encode(out, nimg)
			if err != nil {
				file.Close()
				out.Close()
				return err
			}
		}

		file.Close()
	}

	return nil
}
