import re
f=open('comparative_indoeuropean_database.txt','r')

languages={}
groups={}
sure_relations={}
doubtful_relations={}
start=True
last_unique=0
for line in f:
  if line[0] == 'a' or start == True:
    start = False
    gloss = line.strip().split(None)[2]
    groups[gloss] = {}
    sure_relations[gloss] = []
    doubtful_relations[gloss] = []
  elif line[0] == 'b':
    gloss_id = line.strip().split()[-1]
    groups[gloss][gloss_id] = {}
  elif line[0] == 'c':
    (ida,rel,idb) = line.strip().split()[1:]
    if rel == "2":
      sure_relations[gloss].append((ida,idb))
    elif rel == "3":
      doubtful_relations[gloss].append((ida,idb))
    else: assert False,(line,rel)
  elif line[0:2] == "  ":
    my_gloss_id = gloss_id
    if int(gloss_id) == 1: 
      my_gloss_id = gloss_id + "-" + str(last_unique)
      groups[gloss][my_gloss_id] = {}
      last_unique += 1
    m = re.search("  ([0-9]+) ([0-9]+) (([A-Z\.a-z]+ )+) +(.*)",line.rstrip())
    if m:
      (id,langNum,langName,forms) = m.group(1),m.group(2),m.group(3),m.group(5)
      languages[langName.strip().replace(".","")] = langNum
      if "." not in forms:
        groups[gloss][my_gloss_id][langName.strip().replace(".","")] = forms.split(",")[0].split("/")[0].split(" ")[0].split("(")[0].replace("\n","")



sorted_langs = sorted(languages.keys(),key=lambda l: languages[l])

print sorted_langs

#for gloss in sure_relations:
#  for (a,b) in sure_relations[gloss]:
#    if a in groups[gloss] and b in groups[gloss][b]:
#      groups[gloss][a].update(groups[gloss][b])
#      del groups[gloss][b]
#
#print "#id\t" + "\t".join(map(lambda s: s.replace(" ",""),sorted_langs))

print groups.keys()


#for gloss in groups:
#  for group in groups[gloss]:
#    if len(groups[gloss][group]) != 0:
#      my_line = [ groups[gloss][group][lang] if lang in groups[gloss][group] else "?" for lang in sorted_langs]
#      assert len([x for x in my_line if " " in x or "\n" in x ]) == 0
#      print "%s(%s)\t%s" % (gloss,group,"\t".join(my_line))




