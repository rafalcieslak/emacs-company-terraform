# company-terraform

Company backend for terraform files.

[<img src="https://asciinema.org/a/132870.png" alt="asciinema" width="480"/>](https://asciinema.org/a/132870)

## Features

Autompletes:

 - Arguments in data and resoruce blocks
 - Resource type in data and resource declarations
 - Built-in functions in interpolations
 - Resource and data types in interpolations
 - Resource and data arguments and attributes in interpolations
 - Some top-level keywords

## Usage

To enable `company-terraform`, call `M-x company-terraform-init`, or add

```
(require 'company-terraform)
(company-terraform-init)
```

to your `init.el`.

Completions will appear when both `company-mode` and `terraform-mode` are active.

I recommend also enabling [`company-quickhelp`](https://github.com/expez/company-quickhelp),
which conviniently previews documentation string for the selected completion candidate.

`company-terraform` uses the data provided by the [official Terraform documentation](https://www.terraform.io/docs/).
