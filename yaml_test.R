library(yaml)

cfg = list()
#adding new entries
cfg$version = "v0.00"
cfg[["foo"]] = 10:14 #note quotes
cfg$bar = list(a=7, b="cat", "pi")

yaml::write_yaml(cfg, "yaml_test.yaml")

cfg2 = yaml::read_yaml("yaml_test.yaml")
