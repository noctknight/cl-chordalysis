(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Petitjean's version (from EntropyComputer.java)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

/**
 * This class aims at computing multiple entropies between different sets of
 * variables. This class uses different memoizations and memorizations
 * techniques to retrieve results very quickly.
 */

public class EntropyComputer {
	public static long nbCellsEverParsed;
	HashMap<BitSet, Double> lookup;
	double[] partialEntropy;

	Lattice lattice;
	int nbInstances;

	/**
	 * Constructor
	 * 
	 * @param nbInstances
	 *            number of lines in the database
	 * @param lattice
	 *            associated lattice
	 */
	public EntropyComputer(int nbInstances, Lattice lattice) {
		this.lookup = new HashMap<BitSet, Double>();
		this.lattice = lattice;
		this.nbInstances = nbInstances;
		lookup.put(new BitSet(lattice.getNbVariables()), 0.0);
		this.partialEntropy = new double[this.nbInstances + 1];
		double lnN = log(nbInstances);
		partialEntropy[0] = 0.0;
		for (int i = 1; i < partialEntropy.length; i++) {
			partialEntropy[i] = i * (log(i) - lnN);
		}
		nbCellsEverParsed = 0;
	}

	/**
	 * Computes the partial entropy for a lattice node (set of variables)
	 * 
	 * @param clique
	 *            the lattice node represented by a set of integers
	 * @return the entropy
	 */
	public Double computeEntropy(BitSet clique) {
		Double computedEntropy = lookup.get(clique);
		if (computedEntropy != null) {
//			System.out.println("cached entropy for clique "+clique+":"+clique.hashCode());
			return computedEntropy;
		}
//		System.out.println("Getting entropy for clique "+clique+":"+clique.hashCode());
//		System.out.println("computing entropy for clique "+clique);

		double entropy = 0.0;
		LatticeNode node = lattice.getNode(clique);
		int nbCells = node.getNbCells();
		nbCellsEverParsed += nbCells;
//		 System.out.println("matrix:"+Arrays.toString(matrix));
		for (int i = 0; i < nbCells; i++) {
			int O = node.getMatrixCell(i);
			entropy += partialEntropy[O];
		}
		entropy /= nbInstances;
		entropy *= -1.0;
//		System.out.println("caching "+clique+"("+clique.hashCode()+"):"+entropy);
		lookup.put(clique, entropy);
		return entropy;
	}

	/**
	 * 
	 * @return the number of lines in the database
	 */
	public int getNbInstances() {
		return nbInstances;
	}

	/**
	 * @return the number of variables in the dataset
	 */
	public int getNbVariables() {
		return lattice.getNbVariables();
	} 

|#


#|
	/**
	 * Constructor of a node
	 * 
	 * @param lattice
	 *            the lattice this node belongs to
	 * @param variablesNumbers
	 *            the numbers of the variables corresponding to this node
	 * @param dimensionsForVariables
	 *            the number of values for every variable composing this node
	 * @param records
	 *            the bitsets for all the possible combination of values for the
	 *            variables of this node
	 * @param parents
	 *            the parents of this node
	 */
	public LatticeNode(Lattice lattice, int[] variablesNumbers,
			int[] dimensionsForVariables, BitSet[] records,
			LatticeNode... parents) {
		this.lattice = lattice;
		this.nbVariables = this.lattice.getNbVariables();
		this.variablesNumbers = variablesNumbers;
		this.dimensionsForVariables = dimensionsForVariables;
		this.children = new TreeSet<LatticeNode>();
		nbCells = 1;
		for (int i = 0; i < variablesNumbers.length; i++) {
			nbCells *= dimensionsForVariables[variablesNumbers[i]];
		}
		this.records = records;

		for (LatticeNode parent : parents) {
			parent.addChild(this);
		}
 
		nbCells = 1;
		for (int i = 0; i < variablesNumbers.length; i++) {
			nbCells *= dimensionsForVariables[variablesNumbers[i]];
		} 
|#

