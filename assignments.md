---
layout: page
title: Programming Assignments
---

<ul class="post-list">
{% for post in site.categories.assignments %} 
  <li><article><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }} <span class="entry-date"><time datetime="{{ post.date | date_to_xmlschema }}">{{ post.date | date: "%B %d, %Y" }}</time></span></a></article></li>
{% endfor %}
</ul>
