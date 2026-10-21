---
name: "standard-technical-hegelian-explainer"
description: "Explains a research paper, taking into account its references, building a tree of the evolution of the field of knowledge and its questions through a  hegelian dialectic model."
---

Hegelian Explainer Skill
Input: a research paper, by URL or DOI or arxiv number.
Output: a tree diagram visualised as an artifact or html page which can be zoomed in and out for easier viewing 
---
Nodes in the tree: Individual papers referenced by TITLE. There are 4 fields, one is TITLE, other 3 are:
Each node has a THESIS: what the paper aims to solve and the supporting related work
ANTITHESIS: problems with the related work and what is to be rectified or improved in this paper
SYNTHESIS: Methodology used by the paper and its conclusions or product, with special care taken to resolve the ANTITHESIS
---
The root node of the tree is the paper it was originally called on. 
there is a directed edge from a root note to the papers which explain its thesis or antithesis:
Edges in the tree: 
use a BLUE SOLID EDGE to link each node to a paper contributing to its THESIS
use a RED DOTTED EDGE to link each node to a paper contributing to its ANTITHESIS
----
The tree is to be rendered on a white background and all text is to be in black, leaves are to be bordered in black rectangles.  
----
A node can afford to have no further children (leaf node) if it can be explained with the concepts inside existing_knowledge.md or cv.md in a style which a first-year computing undergraduate with some work experience should be able to understand without loss of rigour, specificity or content. In which case, it will be explained in that way as a paragraph and that particular node will not have the hegelian dialectic. it will function like an axiom. 
If a child node cannot be faithfully explained above OR if it is a high-citation research paper, run this skill on that paper and branch out the tree.
You are encouraged to prioritise exactitude and clarity above all else. Conciseness comes next. Be laser-focused on the eureka in the research. Do not hesitate to branch out if it is fit.