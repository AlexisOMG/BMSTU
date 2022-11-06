package main

import (
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"log"
	"os"
	"strings"

	"github.com/google/uuid"
)

var count_name string = "a" + strings.Replace(uuid.NewString(), "-", "_", -1)

func insertDeclaration(file *ast.File) {
	ast.Inspect(file, func(n ast.Node) bool {
		if blockStmt, ok := n.(*ast.BlockStmt); ok {
			tmpStmt := make([]ast.Stmt, 0, len(blockStmt.List))
			for _, blockNode := range blockStmt.List {
				forStmt, forOk := blockNode.(*ast.ForStmt)
				rangeStmt, rangeOk := blockNode.(*ast.RangeStmt)
				if forOk || rangeOk {
					countObj := ast.Object{
						Kind: ast.Var,
						Name: count_name,
						Data: 0,
					}
					valueSpec := ast.ValueSpec{
						Names: []*ast.Ident{
							{
								Name: count_name,
								Obj:  &countObj,
							},
						},
						Type: &ast.Ident{
							Name: "int",
						},
						Values: []ast.Expr{
							&ast.BasicLit{
								Kind:  token.INT,
								Value: "0",
							},
						},
					}
					valueSpec.Names[0].Obj.Decl = &valueSpec

					declStmt := ast.DeclStmt{
						Decl: &ast.GenDecl{
							Tok: token.VAR,
							Specs: []ast.Spec{
								&valueSpec,
							},
						},
					}
					tmpStmt = append(tmpStmt, &declStmt)
					stmts := []ast.Stmt{
						&ast.AssignStmt{
							Lhs: []ast.Expr{
								&ast.Ident{
									Name: count_name,
									Obj:  &countObj,
								},
							},
							Tok: token.ADD_ASSIGN,
							Rhs: []ast.Expr{
								&ast.BasicLit{
									Kind:  token.INT,
									Value: "1",
								},
							},
						},
					}

					if forOk {
						forStmt.Body.List = append(stmts, forStmt.Body.List...)
					}
					if rangeOk {
						rangeStmt.Body.List = append(stmts, rangeStmt.Body.List...)
					}
					tmpStmt = append(tmpStmt, blockNode, &ast.ExprStmt{
						X: &ast.CallExpr{
							Fun: &ast.Ident{
								Name: "println",
							},
							Args: []ast.Expr{
								&ast.Ident{
									Name: count_name,
									Obj:  &countObj,
								},
							},
						},
					})
				} else {
					tmpStmt = append(tmpStmt, blockNode)
				}
			}
			blockStmt.List = tmpStmt
		}
		return true
	})
}

func main() {
	fset := token.NewFileSet()

	file, err := parser.ParseFile(fset, os.Args[1], nil, parser.ParseComments)
	if err != nil {
		log.Fatal(err.Error())
	}
	// ast.Fprint(os.Stdout, fset, file, nil)
	insertDeclaration(file)
	err = format.Node(os.Stdout, fset, file)
	if err != nil {
		log.Fatal(err.Error())
	}
}
