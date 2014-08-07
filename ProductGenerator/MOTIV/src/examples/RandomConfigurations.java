package examples;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.variables.integer.IntDomainVar;
import es.us.isa.FAMA.parser.FMFParser;
import fr.familiar.attributedfm.AttributedFeatureModel;
import fr.familiar.attributedfm.reasoning.ChocoReasoner;

public class RandomConfigurations {

	private static final String INPUT_DIR = "../../AFMSynthesis/input/betty_generated";
	private static final String OUTPUT_DIR = "../../AFMSynthesis/input/matrices/";
	
	public static void main(String[] args) throws Exception {
		
		File bettyGeneratedModelsDir = new File(INPUT_DIR);
		
		RandomConfigurations generator = new RandomConfigurations();
		for (File inputFile : bettyGeneratedModelsDir.listFiles()) {
			String inputName = inputFile.getName();
			String outputName = inputName.substring(0, inputName.length() - 4) + ".csv"; 
			String outputPath = OUTPUT_DIR + outputName;
			File outputFile = new File(outputPath);
			System.out.println("Generating products for " + inputName);
			generator.generateProducts(inputFile, outputFile);
		}
		generator.generateProducts(new File("../../AFMSynthesis/input/test.afm"), new File("../../AFMSynthesis/input/test.csv"));
		
	}		
	
	public void generateProducts(File inputFile, File outputFile) throws IOException, ContradictionException {
		
		// Products and features
		List<Map<String, String>> products = new ArrayList<Map<String,String>>();
		Set<String> features = new HashSet<String>();
		Map<String, String> types = new HashMap<String, String>();
				
		// Parsing
		FMFParser famaParser = new FMFParser();
		AttributedFeatureModel model = famaParser
				.parseModel(inputFile.getAbsolutePath());

//		 VMReader reader = new VMReader();
//		 AttributedFeatureModel model = (AttributedFeatureModel)
//		 reader.parseFile("../../AFMSynthesis/input/test.vm");


		// Solver
		ChocoReasoner reasoner = new ChocoReasoner();
		model.transformto(reasoner);
		
		Solver solver = new CPSolver();
		Model prob = reasoner.getProblem();
		solver.read(prob);
		solver.propagate();

		// Compute products and features
		int nbSolutions = 0;
		if (solver.solve() == Boolean.TRUE && solver.isFeasible()) {
			do {
				Map<String, String> product = new HashMap<String, String>();
				
				for (int i = 0; i < prob.getNbIntVars(); i++) {
					IntDomainVar aux = solver.getVar(prob.getIntVar(i));
					String name = aux.getName();
					boolean isInternalVar = name.equals("rel-4_card");
					
					if (!isInternalVar) {
						features.add(name);
						product.put(name, String.valueOf(aux.getVal()));
						
						boolean isAttribute = name.contains(".");
						if (isAttribute) {
							types.put(name, "INT(" + Integer.SIZE + ")");
						} else {
							types.put(name, "BOOLEAN");
						}
					}
				}
				
				products.add(product);
				nbSolutions++;
				
			} while (solver.nextSolution() == Boolean.TRUE);
		}
		
		// Writing products to CSV file
		FileWriter writer = new FileWriter(outputFile);
		boolean first = true;
		for (String feature : features) {
			if (first) {
				first = false;
			} else {
				writer.write(",");
			}
			writer.write(feature);			
		}
		
		writer.write("\n");
		
//		first = true;
//		for (String feature : features) {
//			if (first) {
//				first = false;
//			} else {
//				writer.write(",");
//			}
//			String type = types.get(feature);
//			writer.write(type);			
//		}
//
//		writer.write("\n");
		
		for (Map<String, String> product : products) {
			first = true;
			for (String feature : features) {
				
				if (first) {
					first = false;
				} else {
					writer.write(",");
				}
				
				String value = product.get(feature);
				writer.write(value);
			}
			writer.write("\n");
		}
		
		
		writer.close();
	}

}
