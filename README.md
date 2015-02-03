# PostScript Utilities on Chicken-Scheme
chicken-schemeからPostScriptを吐きだします
## USAGE

```
   $ chicken-install

   csi> (use postscheme)

```

## 色々
### (easy-fontset font size)

```
    (define (easy-fontset font size)  
		(findfont font)  
        (scalefont size)  
        (setfont))  
```

オリジナルな関数はこれくらい



### (EPS filename width height expression ...)
これでEPSファイルを出力します  
widthとheightはBoundingBoxのアレです

