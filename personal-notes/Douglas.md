I think their research taste is good in a way that I think like no one's research taste is good.
No bra.
No bra also has good research taste, but no bra where they.
1:25:51
Very.
Clearly understand this dance between the hardware systems that you're like designing the models around and the sort of like algorithmic the side of it.
And this is manifest in the way that the models give this sense of like being, being perfectly designed up to their constraints.
1:26:11
And you can really very clearly see what constraints they're thinking about as they're iteratively solving these problems.
And so I mean, let's take the base transformer and diff that to deep sea V2 and V3.
You can see them running up against that, the memory bandwidth bottleneck in attention and you can see them initially they do MLA to do this.
1:26:34
They trade flops for memory bandwidth basically.
And then they do this thing called NSA where they more selectively load.
And you can see actually this is because the model that they trained with MLA was on H8 hundreds.
So it has a lot of flops.
So they were like, OK, we can freely use the flops, but then the export controls so from like from Biden came in or like they had less of they knew they would have less of those chips going forward.
1:27:01
And so they, they traded off to like a more memory bandwidth oriented like algorithmic solution there.
And you see a similar thing with their approach to sparsity where they're like iteratively working out the best way to do this over multiple papers.
And the part that I like is that it's simple a big failure mode that a lot of MO researchers have is like you do these like overly complicated things that don't like think hard enough about the hardware systems that you have in mind, Whereas the deep, the first DeepSeek like sparsity Moe solution.
1:27:35
They designed these like rack and like like like node level load balancing losses.
So you can see them being like, OK, like we have to like perfectly balanced on this.
And then they actually come up with a much better solution later on where they don't have to have the auxiliary loss where they they just have these like bias terms that they put in and it's.
1:27:55
Isn't it less simple?
Like you're manually putting in a bias rather than but.
Balancing auxiliary loss is annoying.
Like you're you're making the model like trade off this thing and like you have to with auxiliary losses, you have to like control the coefficient and the waiting.
The bias is like cleaner in some respects.