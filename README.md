Done:
1. 数据读取
2. missing value的处理
3. 使用了dummy
4. 删除了cancel=-1
5. run LR model get results


Decided but not done:
1. 需要用一种方法来解决数据unbalance的问题
2. 需要查看correlation
3. 需要做train validation test split


Need to decided:
1. what is c stat
2. why c stat is high
3. 



1. 需要给其他方法加一个train的AUC和validation的AUC来看预测的效果。如果单单是test这个数据的可能会overfitting或者看不出来很多东西，比如svm的train组的AUC肯定比LR的要低，但是validation肯定不好，这就是overfitting的问题。
2. 下一个要点在于商业insight。比如A，看一下coefficient，哪个高，可能就是容易cancel的原因，甚至总结出一个规律，比如：住黄色房子的有3个小孩的女的更容易cancel。形成这样一个模式，好让公司接下去可以针对这一人群做survey什么的。B，看一下category数据之间的关系，如果不同分组对结果影响不大，说明这个分组不太有效果，那么是否以后不需要再收集这类信息。或者效果很大，比如根据zipcode不同地点的人们会有很大的差异，那么说明公司要去不同的地方了解具体信息。
3. 可以考察一下需不需要增加其他项。比如age的平方项进入模型，一般可以看AIC的值，如果变化超过10%，是一个很好的值。如果再看一下residual的分布，我觉得这就是把多重线性回归做到极致了。也没什么好再加的了。
4. 对于新的其他的模型，我真的不知道怎么去说，svm这类模型，很容易overfitting，而且关键是这次的情况svm并没有优于很多，很奇怪。我们先交上去，看看他们怎么说，说起来是模型上还要加强还是feature处理上需要加强还是其他什么的地方需要加强
