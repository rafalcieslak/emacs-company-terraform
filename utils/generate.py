#!/usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import json

provider_list = []
resource_list = []
functions_list = []

header = """
;;; company-terraform-data.el --- Terraform documentation as elisp lists and hashes

;; THIS FILE WAS AUTOMATICALLY GENERATED. DO NOT EDIT.

;;; Code:

(defconst company-terraform-toplevel-keywords '(
    ("resource" "Defines a new resource")
    ("variable" "Defines a variable or module input")
    ("data" "Defines a new data source")
    ("output" "Defines an output value or module output")
    ))

(defconst company-terraform-interpolation-extra '(
    ("module." "References a module")
    ("var." "References a variable")
    ("data." "References a data source")
    ("count." "Resource index metadata")
    ))

(defconst company-terraform-resource-extra '(
    ("count" "count (int) - The number of identical resources to create. This doesn't apply to all resources.")
    ("depends_on" "depends_on (list of strings) - Explicit dependencies that this resource has. These dependencies will be created before this resource.")
    ("provider" "provider (string) - The name of a specific provider to use for this resource. The name is in the format of TYPE.ALIAS, for example, aws.west. Where west is set using the alias attribute in a provider.")
    ("lifecycle" "Customizes the lifecycle behavior of the resource.")
    ))

(defconst company-terraform-data-extra '(
    ("count" "count (int) - The number of identical resources to create. This doesn't apply to all resources.")
    ("depends_on" "depends_on (list of strings) - Explicit dependencies that this resource has. These dependencies will be created before this resource.")
    ("provider" "provider (string) - The name of a specific provider to use for this resource. The name is in the format of TYPE.ALIAS, for example, aws.west. Where west is set using the alias attribute in a provider.")
    ))

(defconst company-terraform-count-extra '(
    ("index" "index (int) - Current counted resource index.")
    ))

(defconst company-terraform-resource-arguments-hash
      (make-hash-table :test 'equal))
(defconst company-terraform-data-arguments-hash
      (make-hash-table :test 'equal))
(defconst company-terraform-resource-attributes-hash
      (make-hash-table :test 'equal))
(defconst company-terraform-data-attributes-hash
  (make-hash-table :test 'equal))
"""

footer = """
(provide 'company-terraform-data)
;;; company-terraform-data.el ends here
"""

def get_sibling(element):
    sibling = element.next_sibling
    if not sibling:
        return sibling
    if sibling == "\n":
        return get_sibling(sibling)
    else:
        return sibling

escaping = str.maketrans({"\"": "\\\"",
                          "\\": "\\\\",
                          "\n":  "\\n"})
s = requests.Session()

def get_providers():
    page = s.get("https://www.terraform.io/docs/providers/index.html")
    assert(page.status_code is 200)
    soup = BeautifulSoup(page.content, 'html.parser')
    li = soup.find_all('li', class_='active')[0]
    lis = li.find_all('li')
    for li in lis:
        href = li.a['href']
        provider = href.split('/')[3]
        provider_list.append(provider)
    

def get_resources_by_provider(provider):
    page = s.get("https://www.terraform.io/docs/providers/%s/index.html" % provider)
    assert(page.status_code is 200)
    soup = BeautifulSoup(page.content, 'html.parser')
    uls = soup.find_all('ul', class_='nav-visible')
    return [(a.get_text(), a['href']) for ul in uls for a in ul.find_all('a')]

def arghelper(kind, soup):
    arglist = []
    argumentrefs = soup.find_all('h2', id='%s-reference' % kind)
    if len(argumentrefs) > 0:
        argumentref = argumentrefs[0]
        ul = get_sibling(argumentref)
        while ul is not None:
            if ul.name == 'ul':
                lis = ul.find_all('li', recursive=False)
                for li in lis:
                    codes = li.find_all('code')
                    if len(codes) > 0:
                        argname = codes[0].get_text()
                    else:
                        aas = li.find_all('a')
                        if len(aas) is 0:
                            continue
                        if aas[0].get('name') is not None:
                            argname = aas[0]['name']
                        else:
                            argname = li.get_text().split('-')[0].strip()
                    argdoc = li.get_text().replace("\n", " ").strip()
                    arglist.append({'name': argname, 'doc': argdoc.translate(escaping)})
                break
            if ul.name == 'h2':
                break
            ul = get_sibling(ul)
    return arglist

def get_resource_params(name, docpath):
    page = s.get("https://www.terraform.io%s" % docpath)
    kind = {'r': 'resource', 'd': 'data', 'external': 'data', 'http': 'data'}[docpath.split('/')[-2]]
    assert(page.status_code is 200)
    soup = BeautifulSoup(page.content, 'html.parser')
    headers = soup.find_all('h1', id=name)
    if len(headers) is 0:
        # Sometimes docs are bonkers and use a wring header.
        headers = soup.find_all('h1')
    header = headers[0]
    maindoc = ""
    p = get_sibling(header)
    while p and p.name != 'h2':
        maindoc += p.get_text().replace("\n", " ") + "\n\n"
        p = get_sibling(p)
    maindoc = maindoc.strip()

    arglist  = arghelper('argument', soup)
    attrlist = arghelper('attributes', soup)

    return {
        'name': name,
        'doc': maindoc.translate(escaping),
        'args': arglist,
        'attrs': attrlist,
        'type': kind
    }

def get_interpolation_functions():
    page = s.get("https://www.terraform.io/docs/configuration/interpolation.html")
    soup = BeautifulSoup(page.content, 'html.parser')
    header = soup.find_all('h2', id='built-in-functions')[0]
    ul = get_sibling(header)
    while ul.name != 'ul':
        ul = get_sibling(ul)
    lis = ul.find_all('li', recursive=False)
    for li in lis:
        aas = li.find_all('a')
        if len(aas) is 0:
            continue
        funcsig = aas[1].get_text()
        funcname = funcsig.split("(")[0]
        funcparam = funcsig[len(funcname):]
        funcdoc = li.get_text().replace("\n", " ").strip()
        functions_list.append({'name': funcname, 'doc': funcdoc.translate(escaping), 'param': funcparam})
    
    

def gather_all_by_provider(provider):
    global resource_list
    print("Gathering resources for %s" % provider)
    resources = get_resources_by_provider(provider)
    for name, docpath in resources:
        print(name)
        params = get_resource_params(name, docpath)
        resource_list.append(params)

def prepare_file():
    print("Generating file...")
    data = ""
    
    # First, build up bare resource list.
    reslist = ""
    for resource in resource_list:
        if resource['type'] == 'resource':
            reslist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                'name': resource['name'],
                'doc': resource['doc']}
    data += "(defconst company-terraform-resources-list '(\n" + reslist + "   ))\n\n"
    
    # Then, resource argument hashes.
    for resource in resource_list:
        if resource['type'] == 'resource':
            arglist = ""
            for argument in resource['args']:
                arglist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                    'name': argument['name'],
                    'doc': argument['doc']}
            data += "(puthash \"%(name)s\" '(\n%(arglist)s\n  ) company-terraform-resource-arguments-hash)\n\n" % {
                'name': resource['name'],
                'arglist': arglist}

    # Then, resource attribute hashes.
    for resource in resource_list:
        if resource['type'] == 'resource':
            attrlist = ""
            for attribute in resource['attrs']:
                attrlist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                    'name':attribute['name'],
                    'doc': attribute['doc']}
            data += "(puthash \"%(name)s\" '(\n%(attrlist)s\n  ) company-terraform-resource-attributes-hash)\n\n" % {
                'name': resource['name'],
                'attrlist': attrlist}
            
    # Data list.
    reslist = ""
    for resource in resource_list:
        if resource['type'] == 'data':
            reslist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                'name': resource['name'],
                'doc': resource['doc']}
    data += "(defconst company-terraform-data-list '(\n" + reslist + "   ))\n\n"
    
    # Then, data argument hashes.
    for resource in resource_list:
        if resource['type'] == 'data':
            arglist = ""
            for argument in resource['args']:
                arglist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                    'name': argument['name'],
                    'doc': argument['doc']}
            data += "(puthash \"%(name)s\" '(\n%(arglist)s\n  ) company-terraform-data-arguments-hash)\n\n" % {
                'name': resource['name'],
                'arglist': arglist}

    # Then, data attribute hashes.
    for resource in resource_list:
        if resource['type'] == 'data':
            attrlist = ""
            for attribute in resource['attrs']:
                attrlist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
                    'name':attribute['name'],
                    'doc': attribute['doc']}
            data += "(puthash \"%(name)s\" '(\n%(attrlist)s\n  ) company-terraform-data-attributes-hash)\n\n" % {
                'name': resource['name'],
                'attrlist': attrlist}

    funclist = ""
    for func in functions_list:
        funclist += "    (\"%(name)s\" \"%(doc)s\")\n" % {
            'name':func['name'],
            'doc': func['doc']}
    data += "(defconst company-terraform-interpolation-functions  '(\n" + funclist + "   ))\n\n"
        
    with open("company-terraform-data.el", "w") as file:
        file.write(header + data + footer)
    
get_providers()
#for p in ['alicloud']:
for p in provider_list:
    gather_all_by_provider(p)
get_interpolation_functions()
prepare_file()
