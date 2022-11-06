\documentclass [12pt]{article}


\usepackage{ucs}
\usepackage[utf8x]{inputenc} %Поддержка UTF8
\usepackage{cmap} % Улучшенный поиск русских слов в полученном pdf-файле
\usepackage[english,russian]{babel} %Пакет для поддержки русского и английского языка
\usepackage{graphicx} %Поддержка графиков
\usepackage{float} %Поддержка float-графиков
\usepackage[left=20mm,right=15mm, top=20mm,bottom=20mm,bindingoffset=0cm]{geometry}
\usepackage{mathtools} 
\usepackage{setspace,amsmath}
\usepackage{amsmath,amssymb}
\usepackage{dsfont}
\usepackage{ragged2e}
\DeclarePairedDelimiter{\abs}{\lvert}{\rvert}
\renewcommand{\baselinestretch}{1.5}
 
\usepackage{color} 
\definecolor{deepblue}{rgb}{0,0,0.5}
\definecolor{deepred}{rgb}{0.6,0,0}
\definecolor{deepgreen}{rgb}{0,0.5,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}

\DeclareFixedFont{\ttb}{T1}{txtt}{bx}{n}{12} % for bold
\DeclareFixedFont{\ttm}{T1}{txtt}{m}{n}{12}  % for normal

\usepackage{listings}

 
\lstset{
	language=Go,
	basicstyle=\ttm,
	otherkeywords={self},             % Add keywords here
	keywordstyle=\ttb\color{deepblue},
	emph={MyClass,__init__},          % Custom highlighting
	emphstyle=\ttb\color{deepred},    % Custom highlighting style
	stringstyle=\color{deepgreen},
	frame=tb,                         % Any extra options here
	showstringspaces=false            % 
}
 
\usepackage{hyperref}
 
\hypersetup{
    bookmarks=true,         % show bookmarks bar?
    unicode=false,          % non-Latin characters in Acrobat’s bookmarks
    pdftoolbar=true,        % show Acrobat’s toolbar?
    pdfmenubar=true,        % show Acrobat’s menu?
    pdffitwindow=false,     % window fit to page when opened
    pdfstartview={FitH},    % fits the width of the page to the window
    pdftitle={My title},    % title
    pdfauthor={Author},     % author
    pdfsubject={Subject},   % subject of the document
    pdfcreator={Creator},   % creator of the document
    pdfproducer={Producer}, % producer of the document
    pdfkeywords={keyword1} {key2} {key3}, % list of keywords
    pdfnewwindow=true,      % links in new PDF window
    colorlinks=true,       % false: boxed links; true: colored links
    linkcolor=black,          % color of internal links (change box color with linkbordercolor)
    citecolor=green,        % color of links to bibliography
    filecolor=magenta,      % color of file links
    urlcolor=cyan           % color of external links
}


\title{}
\date{}
\author{}

\begin{document}
\justifying
\setlength{\parindent}{1.25cm}
\begin{titlepage}
\thispagestyle{empty}
\begin{center}
Федеральное государственное бюджетное образовательное учреждение высшего профессионального образования \\Московский государственный технический университет имени Н.Э. Баумана

\end{center}
\vfill
\centerline{\large{Лабораторная работа №6}}
\centerline{\large{по курсу <<Численные методы>>}}
\centerline{\large{<<Решение систем нелинейных уравнений методом Ньютона>>}}
\centerline{\large{Вариант 9}}
\vfill
\hfill\parbox{5cm} {
           Выполнил:\\
           студент группы ИУ9-62Б \hfill \\
           Егоров Алексей\hfill \medskip\\
       }
\centerline{Москва, 2022}
\clearpage
\end{titlepage}

\textsc{\textbf{1. Цель}} 

Изучение метода Ньютона для решения систем нелинейных уравнений. 

\textsc{\textbf{2. Постановка задачи}}

\textbf{Дано:}  
Система нелинейных уравнений:
\begin{equation*}
\begin{cases}
f_1(x_1, x_2, ..., x_n)=0\\
f_2(x_1, x_2, ..., x_n)=0 \\
… \\
f_n(x_1, x_2, ..., x_n)=0 
\end{cases}
\end{equation*}

\textbf{Найти:}  $\overline{x}=(x_1, ..., x_n)$ - решение системы нелинейных уравнений

\textbf{Индивидуальный вариант:} 

\begin{equation*}
\begin{cases}
cos(x+0.5)-y=2\\
siny-2x=1
\end{cases}
\end{equation*}

\textsc{\textbf{3. Теоретические сведения}}

Представим систему нелинейных уравнений в векторном виде $f(\overline{x})=0$, где $\overline{x}=(x_1, ..., x_n)$ - вектор неизвестных, $f=(f_1, f_2, ..., f_n)$ - вектор функция.

Выбрав начальное приближение $x^0=(x^0_1,x^0_2,...,x^0_n)$ к решению систему, следующие приближение в методе Ньютона строим по рекурентной зависимости:
$$x^{k+1}=x^k-(f'(x^k))^{-1}f(x^k), k=0,1,2...$$ где $(f'(x^k))^{-1}$ - матрица, обратная матрице Якоби.

Таким образом решение для системы уравнений из индивидульного варианта получается следующим:
\begin{equation*}
\begin{cases}
f_1(x,y)=cos(x+0.5)-y-2\\
f_2(x,y)=siny-2x-1
\end{cases}
\end{equation*}
$$f = \left(\begin{array}{c} 
f_1(x,y) \\
f_2(x,y) \\
\end{array}\right)$$


\textbf{3.1 Метод прогонки}

Метод прогонки используется для решения систем линейных уравнений вида $ A\overline{x}=\overline{d}$, где $A$ - трёхдиагональная матрица. Представляет собой вариант метода последовательного исключения неизвестных.

\textbf{Описание алгоритма:}

Пусть массив $a$ - элементы матрицы A под диагональю, $b$ - на диагонали, $c$ – над диагональю.
$$\left(\begin{array}{cccccc} 
b_1 & c_1 & 0 & ... & ... &  0 \\
a_1 & b_2 & c_2 & ... & ... &  0 \\
0 & a_2 & b_3 & c_3 & ... &  0 \\
\vdots & ... & \ddots & \ddots & \ddots &  \vdots \\
0 & ... & ... & a_{n-2} & b_{n-1} & c_{n-1} \\
0 & ... & ... & ... & a_{n-1} & b_n \\
\end{array}\right) $$

Соответствующая СЛАУ:
\begin{equation*}
\begin{cases}
b_1x_1+c_1x_2=d_1\\
a_1x_1+b_2x_2+c_2x_3=d_2 \\
… \\
a_{n-1}x_{n-1}+b_nx_n=d_n 
\end{cases}
\end{equation*}

$x_i$ вычисляется следующим образом:
% $$x_{i-1}=\alpha_{i-1}x_i+\beta_{i-1},i=\overline{2,n}$$
$$x_i=\alpha_ix_{i+1}+\beta_i,i=\overline{n-1,1}$$
$$x_n=\frac{d_n-a_{n-1}b_{n-1}}{a_{n-1}\alpha_{n-1}+b_n}$$

$\alpha_i,\beta_i$ вычисляются следующим образом:
$$\alpha_i=-\frac{c_i}{a_{i-1}\alpha_{i-1}+b_i},i=\overline{2, n-1}$$
$$\beta_{i} = \frac{d_i-a_{i-1}\beta_{i-1}}{a_{i-1}\alpha_{i-1}+b_i},i=\overline{2,n}$$
$$ \alpha_1=\frac{c_1}{b_1},\beta_1=\frac{d_1}{b_1}  $$

Вычисление $\alpha_i,\beta_i$ называется прямым ходом метода прогонки. Вычисление $x_i$ - обратным ходом метода прогонки.

Достаточные условия метода:
\begin{enumerate}
\item $\abs{b_{i}} \geq \abs{a_{i-1}} + \abs{c_{i}} $, $i=\overline{2,n-1}$ - без него возможно решение, но может и не быть.
\item $\abs{d_{i}} >\abs{c_{i}} $, $i=\overline{2,n-1}$
\end{enumerate}

\textbf{3.2 Метод Гаусса}

Метод Гаусса заключается в последовательном исключении переменных: с помощью элементарных преобразований система уравнений приводится к равносильной системе треугольного вида, из которой последовательно, начиная с последних (по номеру), находятся все переменные системы.

\textbf{Описание алгоритма:}

Пусть дана система:

\begin{equation*}
\begin{cases}
a_{1, 1}x_{1} + ... + a_{1, n}x_{n} = d_{1} \\
... \\
a_{i, 1}x_{1} + ... + a_{i, n}x_{n} = d_{i} \\
... \\
a_{n, 1}x_{1} + ... + a_{n, n}x_{n} = d_{n}
\end{cases}
\end{equation*}

Приведем матрицу коэффициентов к верхнетреугольному виду:
На первом шаге вычитаем из i-ой строки, где $i=2...n$, первую строку, домноженную на $\frac{a_{i, 1}}{a_{1, 1}}$. Далее аналогичным образом вычитаем вторую строку, в конечном итоге получаем систему вида:

\begin{equation*}
\begin{cases}
a_{1, 1}''x_{1} + ... + a_{1, n}''x_{n} = d_{1}'' \\
... \\
a_{i, i}''x_{1} + ... + a_{i, n}''x_{n} = d_{i}'' \\
... \\
a_{n, n}''x_{n} = d_{n}''
\end{cases}
\end{equation*}

Далее начинается обратной ход метода Гаусса. Вычитаем из i-ой строки, где $i=1...n-1$, последнюю, домноженную на $\frac{a_{i, n}''}{a_{n, n}''}$.

Продолжая этот процесс придём к диагональной матрице:

\begin{equation*}
    \begin{cases}
        a_{1, 1}'''x_{1} = d_{1}''' \\
        ... \\
        a_{i, i}'''x_{i} = d_{i}''' \\
        ... \\
        a_{n, n}'''x_{n} = d_{n}''' \\
    \end{cases}
\end{equation*}

Разделив i-ую строку на $a_{i, i}'''$ получаем решение.\\

\textbf{Оценка погрешности для решения СЛАУ при отсутствии точного решения:}

Найти вектор-решение $\overline{x}^*$ с помощью метода прогонки и вектор $\overline{x}$ с помощью метода Гаусса.

При вычислении $\overline{x}^*$ и $\overline{x}$ с плавающей точкой возникает погрешность:
$$ \overline{\varepsilon} = (\overline{x}^* - \overline{x}) $$

\textsc{\textbf{4. Практическая реализация:}}

Листинг 1. Метод прогонки и метод Гаусса для решения СЛАУ с трёхдиагональной матрицей
\begin{lstlisting}[language=go]

package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

const SIZE = 4

func readCoeff(line string, name string, size int) ([]float64, error) {
	res := make([]float64, 0, size)

	strNums := strings.Split(line, " ")
	if size != len(strNums) {
		return nil, errors.New("wrong size of " + name)
	}

	for _, sNum := range strNums {
		num, err := strconv.ParseFloat(sNum, 64)
		if err != nil {
			return nil, err
		}
		res = append(res, num)
	}

	return res, nil
}

func direct(bs, as, cs, ds []float64) (alpha, beta []float64) {
	alpha = append(alpha, -cs[0]/bs[0])
	beta = append(beta, ds[0]/bs[0])
	var div float64
	for i := 1; i < SIZE-1; i++ {
		div = as[i-1]*alpha[i-1] + bs[i]
		alpha = append(alpha, -cs[i]/div)
		beta = append(beta, (ds[i]-as[i-1]*beta[i-1])/div)
	}
	div = as[SIZE-2]*alpha[SIZE-2] + bs[SIZE-1]
	beta = append(beta, (ds[SIZE-1]-as[SIZE-2]*beta[SIZE-2])/div)
	return alpha, beta
}

func reverse(alpha, beta []float64) (xs []float64) {
	xs = make([]float64, SIZE)
	xs[SIZE-1] = beta[SIZE-1]
	for i := SIZE - 2; i >= 0; i-- {
		xs[i] = alpha[i]*xs[i+1] + beta[i]
	}
	return xs
}

func gauss(matrix [][]float64, ds []float64) (xs []float64) {
	xs = make([]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		for j := i + 1; j < SIZE; j++ {
			var k float64 = matrix[j][i] / matrix[i][i]
			for t := i; t < SIZE; t++ {
				matrix[j][t] -= k * matrix[i][t]
			}
			ds[j] -= k * ds[i]
		}
	}
	for i := SIZE - 1; i >= 0; i-- {
		var k float64 = 0
		for j := i + 1; j < SIZE; j++ {
			k += matrix[i][j] * xs[j]
		}
		xs[i] = (ds[i] - k) / matrix[i][i]
	}
	return xs
}

func multiply(matrix [][]float64, xs []float64) (ds []float64) {
	ds = make([]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		var sum float64 = 0
		for j := 0; j < SIZE; j++ {
			sum += matrix[i][j] * xs[j]
		}
		ds[i] = sum
	}
	return ds
}

func buildMatrix(bs, as, cs []float64) [][]float64 {
	matrix := make([][]float64, SIZE)
	for i := 0; i < SIZE; i++ {
		matrix[i] = make([]float64, SIZE)
	}
	for i := 0; i < SIZE; i++ {
		for j := 0; j < SIZE; j++ {
			if i == j {
				matrix[i][j] = bs[i]
				if i != SIZE-1 {
					matrix[i][i+1] = as[i]
					matrix[i+1][i] = cs[i]
				}
			}
		}
	}
	return matrix
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err.Error())
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if scanner.Err() != nil {
		log.Fatal(scanner.Err().Error())
	}

	bs, err := readCoeff(lines[0], "bs", SIZE)
	if err != nil {
		log.Fatal(err.Error())
	}
	as, err := readCoeff(lines[1], "as", SIZE-1)
	if err != nil {
		log.Fatal(err.Error())
	}
	cs, err := readCoeff(lines[2], "cs", SIZE-1)
	if err != nil {
		log.Fatal(err.Error())
	}
	ds, err := readCoeff(lines[3], "ds", SIZE)
	if err != nil {
		log.Fatal(err.Error())
	}

	firstRes := reverse(direct(bs, as, cs, ds))
	fmt.Print("First: ")
	for _, res := range firstRes {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}

	matrix := buildMatrix(bs, as, cs)
	secondRes := gauss(matrix, ds)
	fmt.Print("\nSecond: ")
	for _, res := range secondRes {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}
	fmt.Print("\nDiff: ")
	for i := 0; i < SIZE; i++ {
		fmt.Print(fmt.Sprintf("%.16f", math.Abs(firstRes[i]-secondRes[i])), " ")
	}
	fmt.Println()
	matrix = buildMatrix(bs, as, cs)
	newDs := multiply(matrix, firstRes)
	fmt.Print("NewDs: ")
	for _, res := range newDs {
		fmt.Print(fmt.Sprintf("%.16f", res), " ")
	}
	fmt.Println()
}



\end{lstlisting}


\textsc{\textbf{5. Результат:}}

Для тестирования полученной программы в качестве трехдиагональной матрицы $A$ была выбрана следующая матрица: 
$$A = \left(\begin{array}{cccc} 
4 & 1 & 0 & 0 \\
1 & 4 & 1 & 0 \\
0 & 1 & 4 & 1 \\
0 & 0 & 1 & 4  
\end{array}\right) $$

В качестве вектора $\overline{d}$: 
$$\overline{d} = \left(\begin{array}{c} 
5 \\
6 \\
6 \\
5 \\
\end{array}\right)$$

В результате работы программы (Листинг 1) получаем значения:

$$\overline{\varepsilon} = \left(\begin{array}{c} 
0 \\
0 \\
0 \\
0 \\
\end{array}\right), \quad \overline{x} = \left(\begin{array}{c} 
1 \\
1 \\
1 \\
1 \\
\end{array}\right), \quad \overline{x}^* = \left(\begin{array}{c} 
1 \\
1 \\
1 \\
1 \\
\end{array}\right)$$

Как видно выше, не всегда вектор $\overline{\varepsilon}$ может содержать погрешность (нулевой вектор ошибок) в связи с использованием типов данных с двойной точностью. 
Протестируем программу на измененном векторе $\overline{d}$: 
$$ \overline{d} =
 \left(\begin{array}{c} 
5 \\
6 \\
6 \\
6 \\
\end{array}\right)
$$

В результате работы программы (Листинг 1) получаем значения:

$$\overline{\varepsilon} = \left(\begin{array}{c} 
0.0000000000000001 \\
0.0000000000000002 \\
0.0000000000000000 \\
0.0000000000000000 \\
\end{array}\right), \quad \overline{x} = \left(\begin{array}{c} 
0.9952153110047848 \\
1.0191387559808611 \\
0.9282296650717703 \\
1.2679425837320575 \\
\end{array}\right), \quad \overline{x}^* = \left(\begin{array}{c} 
0.9952153110047847 \\
1.0191387559808613 \\
0.9282296650717703 \\
1.2679425837320575 \\
\end{array}\right)$$


\textbf{6. Вывод:}

В ходе выполнения лабораторной работы был изучен метод решения СЛАУ с трёхдиагональной матрицей: метод прогонки. Так же были реализованы методы прогонки и Гаусса на языке программирования Go.

Для метода прогонки можно отметить то, что отсутствует методологическая (логическая) погрешность, но присутствует вычислительная погрешность в связи с использованием чисел с плавающей запятой, ведущая к высокому накоплению вычислительной ошибки.


\end{document}  
