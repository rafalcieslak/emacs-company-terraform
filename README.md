# company-terraform

Company backend for terraform files.

[<img src="https://asciinema.org/a/132870.png" alt="asciinema" width="480"/>](https://asciinema.org/a/132870)

## Features

Autompletes:

 - Arguments and meta-parameters in data and resource blocks
 - Resource type in data and resource declarations
 - Top-level keywords
 - In interpolations:
   - Built-in functions
   - Resource and data types and names, conveniently limited to those existing in your project
   - Resource and data arguments and attributes
   - Variable names
   - Meta-parameters and keywords

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
