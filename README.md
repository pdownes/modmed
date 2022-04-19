# modmed
Edwards Moderated Mediation in R

<div class="container-fluid main-container">




<div id="header">



<h1 class="title toc-ignore">Edwards ModMed README</h1>
<h4 class="author">Patrick Downes</h4>
<h4 class="date">4/19/2022</h4>

</div>


<div id="estimate-models" class="section level2">
<h2>Estimate Models</h2>
<p>This helper script runs Edwards &amp; Lambert’s (2007) moderated mediation analysis using basic OLS regressions from Equations 5 and 20 of the article. The first step is to estimate both regression models using lm(). Something like:</p>
<pre class="r"><code class="hljs">lmm &lt;- lm(m ~ {controls} + x + xz + z,data=dat)
lmy &lt;- lm(y ~ {controls} + x + xz + z + m,data=dat)</code></pre>
</div>
<div id="bootstrap-and-calculate-cis" class="section level2">
<h2>Bootstrap and calculate CI’s</h2>
<p>After estimating both models, bootstrap samples frmo the data and recompute the same models. Then compute confidence intervals for those distributions. Note that at present the script only computes the percentile method (the bias-corrected CI would be better in a revised version).</p>
<pre class="r"><code class="hljs"><span class="hljs-keyword">source</span>(<span class="hljs-string">'edwards.modmed.R'</span>)
coefs &lt;- bootstrap(lmm,lmy,dat)
edwards.modmed(coefs=coefs,lmm=lmm,lmy=lmy,zlevels=c(-<span class="hljs-number">1</span>*sd(dat$mcppgo),sd(dat$mcppgo)),p=<span class="hljs-number">.05</span>)</code></pre>
</div>




</div>
