---
layout: post
category: assignments
title: "State-Space Problem Solver: Missionaries and Cannibals"
date: 2014-11-01T09:10:00-05:00
---

A solution to the Missionaries and Cannibals problem using a classic
state-space problem solving approach.

Source Files:

  - [mc.l]({{ site.baseurl }}/assignments/mc/mc.l)
  - [classes.l]({{ site.baseurl }}/assignments/mc/classes.l)
  - [util.l]({{ site.baseurl }}/assignments/mc/util.l)

Graph of state-space solution. Normal states in brown, feast states in red,
goal state in blue. Each arrow represents an operation, such as `MC->RB` which
stands for "1 missionary and 1 cannibal moved to right-bank", or `CC->LB` which
stands for "2 cannibals moved to left-bank". The operations taken to reach
solution state are colored blue.

![Solution]({{ site.baseurl }}/assignments/mc/graph.svg)