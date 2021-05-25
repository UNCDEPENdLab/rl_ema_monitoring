---
title: "{{ replace .Name "-" " " | title }}"
output: html_document
categories: ["subjects"]
tags: ["{{ replace .Name "-" " " | title }}"]
params:
  id: "{{ replace .Name "-" " " | title }}"
---