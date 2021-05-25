---
title: "{{ replace .Name "-" " " | title }} ECG"
output: html_document
aliases:
  - /{{ lower ( replace .Name "-" " " | title ) }}-ecg/
  - /{{ lower ( replace .Name "-" " " | title ) }}/ecg/
categories: ["graphs", "{{ replace .Name "-" " " | title }}", "ECG"]
tags: ["{{ replace .Name "-" " " | title }}"]
params:
  block: 1
  id: "{{ replace .Name "-" " " | title }}"
---