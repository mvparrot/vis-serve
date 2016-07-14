
<style>
.midcenter {
    position: fixed;
    top: 50%;
    left: 50%;
}
</style>



Visualizing Serve Trajectories in High-Performance Tennis with R
=================================
author: Alwin  Wang
date: 18th July 2016
font-family: 'Helvetica'
transition: rotate
transition-speed: slow
#incremental: true

Impact Image
========================================================
title: false
<div class="midcenter" style="margin-left:-400px; margin-top:-300px;">
<img src="hawkeye.jpg"></img>
</div>


Initial Analysis
========================================================
type: section

```
[1] "atp serve data loaded"
[1] "generate organised data functions loaded"
```



Identifying outliers
========================================================
For more details on authoring R presentations click the
**Help** button on the toolbar.

- Bullet 1
- Bullet 2
- Bullet 3

Slide With Code
========================================================

```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```



Slide With Plot
========================================================

```r
plot(cars)
```

![plot of chunk unnamed-chunk-3](Presentation_vis-serve-figure/unnamed-chunk-3-1.png)
