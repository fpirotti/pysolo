---
title: "Troubleshooting RMarkdown to PDF"
author: "Scott Jackson <br/> Frustrated RMarkdown Learner"
date: "July 27, 2024"
output: 
  pdf_document:
    keep_md: true
editor_options:
  chunk_output_type: console
---



## A Simple Test
Running this next chuck works when run interactively, but not when knit to a PDF.

\begin{table}

\caption{\label{tab:test}mtcars example}
\centering
\fontsize{12}{14}\selectfont
\begin{tabular}[t]{l|r|r|r|r}
\hline
  & MPG & \# Cyl & Displacement & Horsepower\\
\hline
Mazda RX4 & 21.0 & 6 & 160 & 110\\
\hline
Mazda RX4 Wag & 21.0 & 6 & 160 & 110\\
\hline
Datsun 710 & 22.8 & 4 & 108 & 93\\
\hline
Hornet 4 Drive & 21.4 & 6 & 258 & 110\\
\hline
Hornet Sportabout & 18.7 & 8 & 360 & 175\\
\hline
\end{tabular}
\end{table}
