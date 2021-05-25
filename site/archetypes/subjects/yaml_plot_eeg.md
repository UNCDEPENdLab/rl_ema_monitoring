---
title: "{{ replace .Name "-" " " | title }} EEG"
output: html_document
aliases:
  - /{{ lower ( replace .Name "-" " " | title ) }}-eeg/
  - /{{ lower ( replace .Name "-" " " | title ) }}/eeg/
categories: ["graphs", "{{ replace .Name "-" " " | title }}", "EEG"]
tags: ["{{ replace .Name "-" " " | title }}"]
params:
  block: 1
  id: "{{ replace .Name "-" " " | title }}"
---