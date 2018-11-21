import requests
from bs4 import BeautifulSoup
import jinja2


PFA_LIBRARY_SPECIFICATION = "https://raw.githubusercontent.com/datamininggroup/pfa/master/libfcns.xml"
TYPE_LOOKUP = {
    "function": "FunctionRef"
}

FUNCTION_TO_OBJECT = {
    "+": "plus",
    "-": "minus",
    "*": "mult",
    "/": "div",
    "//": "divfloor",
    "u-": "addinv",
    "%": "mod",
    "%%": "modmod",
    "&&": "and",
    "^^": "xor",
    "&&&": "nullableAnd",
    "|||": "nullableOr",
    "!!!": "nullableNot",
    "==": "eq",
    "<": "lt",
    "<=": "lte",
    ">": "gt",
    ">=": "gte",
    "!=": "net",
    "**": "pow",
    "!": "not",
    "toString": "toStringPFA"
}

DEFAULT_NAMESPACE = "core"
DEFAULT_TYPE = "Any"
OBJECT_DEF = jinja2.Template("object {{object_name}} { {%for function_def in function_defs %} {{function_def}} {%endfor%}}")
APPLICATION_CALL = jinja2.Template("def apply({%for param in params %} {{param['name']}}: {{param['type']}}{% if not loop.last %},{% endif %}{% endfor %}) = new FunctionCall(\"{{function_name}}\" {% for param in params %} ,{{param['name']}} {% endfor %})")
INDENT_SPACES = 4

OBJECT_TEMPLATE = jinja2.Template(
    '''
    object {{object_name}} {
        {% for object_def in object_defs %}
            {{object_def}}
        {% endfor %}
    }
    '''
)

TOP_LEVEL_OBJECT = jinja2.Template('''
// DO NOT EDIT BY HAND, GENERATED BY pfa_dsl_generator.py
// MODIFIED tag is used for edge case modified by hand, please look for the tag before replacing
// Please format using a scala formatter 
package com.ibm.aardpfark.pfa.functions

import com.ibm.aardpfark.pfa.expression._
import com.ibm.aardpfark.spark.ml.linear.LinearModelData

trait FunctionLibrary {
  {% for object_def in object_defs %}
    {{object_def}}
  {% endfor %}
}

''')


def get_specification(url):
    response = requests.get(url)
    bs = BeautifulSoup(response.content)
    return bs


def get_function_lib(spec):
    return spec.findAll("fcn")

def get_object_name(function_name):
    return FUNCTION_TO_OBJECT.get(function_name,function_name)

def gen_object(object_definition):
    function_name = object_definition.attrs["name"] #type: str
    functions = object_definition.findAll("sig")
    namespace = get_namespace(function_name)
    function_defs = [gen_one_function(function_name,function_def) for function_def in functions]
    # Dedupe function_defs because of multiple declarations in the xml for different types
    function_defs = list(set(function_defs))
    object_name = get_object_name(namespace[-1])
    object_def = OBJECT_DEF.render(object_name = object_name, function_defs = function_defs)
    return {
        "namespace": namespace, #type: List[str]
        "function_name" : function_name , #type: str
        "object_name" : object_name,
        "object_def": object_def
    }

def gen_one_function(function_name,function_definition):
    try:
        params = [{"name": param.attrs["name"], "type": get_type(param)} for param in function_definition.findAll("par",recursive=False)]
        function_call = APPLICATION_CALL.render(params=params,function_name=function_name)
    except Exception as e:
        print([param for param in function_definition.findAll("par",recursive=False)])
        print(function_name)
        print(function_definition)
        raise e
    return function_call

def get_namespace(function_name):
    namespace = function_name.split(".")
    if len(namespace) == 1 :
        return([DEFAULT_NAMESPACE,function_name])
    else:
        return namespace

def get_type(param_def):
    if param_def.findAll("function"):
        pfa_type = "function"
    elif len([type for type in param_def.children]) > 1:
        pfa_type = "union"
    else:
        pfa_type = [type for type in param_def.children][0].name
    scala_type = TYPE_LOOKUP.get(pfa_type,DEFAULT_TYPE)
    return scala_type

def gen_all_objects(func_list):
    return [gen_object(function) for function in func_list]


def build_object_dict(object_list):
    object_dict = {}
    for object_def in object_list:
        dict_handle = object_dict
        num_levels = len(object_def["namespace"])
        for ix, namespace in enumerate(object_def["namespace"]):
            if namespace not in dict_handle.keys():
                dict_handle[namespace] = {}
            if ix+1 == num_levels:
                dict_handle[namespace] = object_def
            else:
                dict_handle = dict_handle[namespace]
    return(object_dict)

def generate_objects(object_dict, obj_template, object_name, indenting=INDENT_SPACES):
    object_defs = []
    top_level_namespaces = object_dict.keys()
    for namespace in top_level_namespaces:
        if "object_def" in object_dict[namespace].keys():
            object_defs.append(object_dict[namespace]["object_def"])
        else:
            object_defs.append(generate_objects(object_dict[namespace],OBJECT_TEMPLATE,namespace))
    return obj_template.render(object_defs=object_defs, object_name =object_name)



def generate_text_file(spec_dict,file_name):
    function_list = get_function_lib(spec_dict)
    object_list = gen_all_objects(function_list)
    object_dict = build_object_dict(object_list)
    scala_text = generate_objects(object_dict, TOP_LEVEL_OBJECT,None)
    with open(file_name,"w") as f:
        f.write(scala_text)


if __name__ == '__main__':
    spec = get_specification(PFA_LIBRARY_SPECIFICATION)
    generate_text_file(spec,'FunctionLibrary.scala')