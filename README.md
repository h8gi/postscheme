# PostScript Utilities on Chicken-Scheme
chicken-schemeからPostScriptをたたきましょう  
## USAGE
(use postscheme)

## 色々
### (easy-fontset font size)

```
    (define (easy-fontset font size)  
		(findfont font)  
        (scalefont size)  
        (setfont))  
```

オリジナルな函数はこれくらい



### (EPS filename width height expression ...)
これでEPSファイルを出力します  
widthとheightはBoundingBoxのアレです

