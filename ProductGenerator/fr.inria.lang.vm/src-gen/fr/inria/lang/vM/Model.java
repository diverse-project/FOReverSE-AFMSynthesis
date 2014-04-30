/**
 */
package fr.inria.lang.vM;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fr.inria.lang.vM.Model#getPackageDeclaration <em>Package Declaration</em>}</li>
 *   <li>{@link fr.inria.lang.vM.Model#getImportDeclarations <em>Import Declarations</em>}</li>
 *   <li>{@link fr.inria.lang.vM.Model#getMetainformation <em>Metainformation</em>}</li>
 *   <li>{@link fr.inria.lang.vM.Model#getVm <em>Vm</em>}</li>
 *   <li>{@link fr.inria.lang.vM.Model#getObjectives <em>Objectives</em>}</li>
 *   <li>{@link fr.inria.lang.vM.Model#getConfigurations <em>Configurations</em>}</li>
 * </ul>
 * </p>
 *
 * @see fr.inria.lang.vM.VMPackage#getModel()
 * @model
 * @generated
 */
public interface Model extends EObject
{
  /**
   * Returns the value of the '<em><b>Package Declaration</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Package Declaration</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Package Declaration</em>' containment reference.
   * @see #setPackageDeclaration(PackageDeclaration)
   * @see fr.inria.lang.vM.VMPackage#getModel_PackageDeclaration()
   * @model containment="true"
   * @generated
   */
  PackageDeclaration getPackageDeclaration();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.Model#getPackageDeclaration <em>Package Declaration</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Package Declaration</em>' containment reference.
   * @see #getPackageDeclaration()
   * @generated
   */
  void setPackageDeclaration(PackageDeclaration value);

  /**
   * Returns the value of the '<em><b>Import Declarations</b></em>' containment reference list.
   * The list contents are of type {@link fr.inria.lang.vM.ImportDeclaration}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Import Declarations</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Import Declarations</em>' containment reference list.
   * @see fr.inria.lang.vM.VMPackage#getModel_ImportDeclarations()
   * @model containment="true"
   * @generated
   */
  EList<ImportDeclaration> getImportDeclarations();

  /**
   * Returns the value of the '<em><b>Metainformation</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Metainformation</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Metainformation</em>' containment reference.
   * @see #setMetainformation(MetaData)
   * @see fr.inria.lang.vM.VMPackage#getModel_Metainformation()
   * @model containment="true"
   * @generated
   */
  MetaData getMetainformation();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.Model#getMetainformation <em>Metainformation</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Metainformation</em>' containment reference.
   * @see #getMetainformation()
   * @generated
   */
  void setMetainformation(MetaData value);

  /**
   * Returns the value of the '<em><b>Vm</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Vm</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Vm</em>' containment reference.
   * @see #setVm(VariabilityModel)
   * @see fr.inria.lang.vM.VMPackage#getModel_Vm()
   * @model containment="true"
   * @generated
   */
  VariabilityModel getVm();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.Model#getVm <em>Vm</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Vm</em>' containment reference.
   * @see #getVm()
   * @generated
   */
  void setVm(VariabilityModel value);

  /**
   * Returns the value of the '<em><b>Objectives</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Objectives</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Objectives</em>' containment reference.
   * @see #setObjectives(Objectives)
   * @see fr.inria.lang.vM.VMPackage#getModel_Objectives()
   * @model containment="true"
   * @generated
   */
  Objectives getObjectives();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.Model#getObjectives <em>Objectives</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Objectives</em>' containment reference.
   * @see #getObjectives()
   * @generated
   */
  void setObjectives(Objectives value);

  /**
   * Returns the value of the '<em><b>Configurations</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Configurations</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Configurations</em>' containment reference.
   * @see #setConfigurations(Configurations)
   * @see fr.inria.lang.vM.VMPackage#getModel_Configurations()
   * @model containment="true"
   * @generated
   */
  Configurations getConfigurations();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.Model#getConfigurations <em>Configurations</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Configurations</em>' containment reference.
   * @see #getConfigurations()
   * @generated
   */
  void setConfigurations(Configurations value);

} // Model
