import dlwh.newcognates._;
import scalanlp.util._;
import ToyDataset._;

val index = Index( ('a' to 'z') :+ '\0')

import ThreeStateEditDistance._;

val costs = simpleCostMatrix(index.size - 1,-4,-4);
expectedCounts(index,"ahon","won",costs);

val ff = new WordFactorsFactory(new scalanlp.fst.fast.AutomatonFactory(index))
import ff._;

val wf = new WordFactors(Set("ahon","won"),-4,-4);
val inf = new TreeElimination(wf,tree,CognateGroup(Cognate("ahon","a",'above),Cognate("won","b",'above)));

inf.edgeMarginal("aa","a")

