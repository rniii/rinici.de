---
title: Playable 88x31s
---

# Playable 88x31 buttons

Recently I was working on a [1kb Tetris game][tetris], and I figured it'd be fun to embed little
games inside 88x31s. So here they are! All of them are golfed, so they're about the same size as a
gif file for a button.

[tetris]: https://codepen.io/222rini/pen/zYgxmmv

## Snake game

<details>
<summary>692 bytes including HTML</summary>

```html
<!doctype html><body style=margin:0;overflow:hidden title='Double click me!'bgcolor=#121
onload='W=29,H=290,n=d=1,c=c.getContext`2d`
q=(p,x)=>c.fillRect?.(1.1+p%W*3,1.1+(p/W|0)*3,2.5,2.5,c.fillStyle=x%2?j:i)
p=[117,,,],u=new Set,setInterval(z=>{[z]=p,u.delete(z)||p.pop()
z%W+d%W<W&&z%W+-~d%W&&!p.includes(z+=d=n)&&z>0&&z<H?p.unshift(z):c=setTimeout(_=>location.reload(),2000)
for(c.clearRect?.(0,0,H,H);u.size<3;p.includes(z=0|Math.random()*H)||u.add(z));c.strokeRect?.(0,0,88,31),[...u].map(q,i=j=`#e00`)
p.map(q,i=`#4d4`,j=`#1a1`)},166)'onkeydown=e=event.keyCode-65,n=d%W?e^22?e^18?d:W:-W:e?e^3?n:1:-1
ondblclick=h.click()><canvas id=c><a id=h target=_blank href=https://rinici.de/buttons>
```
</details>

<iframe src="/buttons/snake" width="88" height="31" sandbox="allow-scripts allow-popups" style="border:none"></iframe>

```html
<iframe src="https://rinici.de/buttons/snake" width="88" height="31" sandbox="allow-scripts allow-popups" style="border:none"></iframe>
```

## Flappy bird

This one was really fun to make. Initially I was going to leave it as just black and white, but it
ended up being so tiny I could actually draw some stuff. Since the width is fixed, I just reset the
Y coords of the pipes every 44th frame, as they're moved every frame. This means only three pipes 
exist in a list at a time, and the second one is always the one right in front of you, so collision
is just a simple bounds check!

<details>
<summary>670 bytes including HTML</summary>

```html
<!doctype html><body style=margin:0;overflow:hidden title='Right click me!'bgcolor=#0ab
onload='c=c.getContext`2d`,f=h=0,u=[],w=9
m=(...i)=>c.fillRect?.(...i)|c.strokeRect?.(...i)
setInterval(z=>{w++>42&&u.shift(w=0)
for(h+=f+=0.15;u.length<3;)u.push(Math.random()*5+Math.random()*5);c.clearRect?.(0,0,99,99)
c.strokeRect?.(0,0,88,31)
c.fillStyle=`#ff0`,m(9,h,5,5),c.fillStyle=`#2f2`
u.map((z,i)=>(m(i=i*44-w,17+z,5,99),m(i,0,5,3+z),m(i-=1,z,7,3),m(i,z+17,7,3)))
h<0||h>25||w>30&&w<39&&(h<u[1]||h>u[1]+12)?c=setTimeout(z=>location.reload(),2000):0},49)'onclick=h-=6,f=0
oncontextmenu='return!!r.click()'><canvas id=c><a id=r target=_blank href=https://rinici.de/buttons>
```
</details>

<iframe src="/buttons/flappy" width="88" height="31" sandbox="allow-scripts allow-popups" style="border:none"></iframe>

```html
<iframe src="https://rinici.de/buttons/flappy" width="88" height="31" sandbox="allow-scripts allow-popups" style="border:none"></iframe>
```

## License

You're free to embed these in your website! As with the website's source they are also available
under the [Apache License, version 2.0](https://www.apache.org/licenses/LICENSE-2.0).
