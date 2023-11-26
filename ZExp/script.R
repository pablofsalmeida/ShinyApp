dados=read.table("ovos.txt", head=T, dec=",") 
attach(dados)

require(ExpDes.pt)
dic(embal, alb, mcomp="sk")

# Função do ExpDes.pt
scottknott(alb, embal, 28, 60.287, alpha = 0.05, group = TRUE, main = NULL)


library(ScottKnott)
SK(lm(alb~embal))$out$Result




#### Teste dunnett
require(asbio)
pairw.anova(alb, embal, control="Fresco", method="dunnett")
		 resp, trat