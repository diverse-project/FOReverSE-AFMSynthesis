/**
 */
package fr.inria.lang.vM;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Variability Model</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link fr.inria.lang.vM.VariabilityModel#getRelationships <em>Relationships</em>}</li>
 *   <li>{@link fr.inria.lang.vM.VariabilityModel#getAttributes <em>Attributes</em>}</li>
 *   <li>{@link fr.inria.lang.vM.VariabilityModel#getDescriptions <em>Descriptions</em>}</li>
 *   <li>{@link fr.inria.lang.vM.VariabilityModel#getConstraints <em>Constraints</em>}</li>
 * </ul>
 * </p>
 *
 * @see fr.inria.lang.vM.VMPackage#getVariabilityModel()
 * @model
 * @generated
 */
public interface VariabilityModel extends EObject
{
  /**
   * Returns the value of the '<em><b>Relationships</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Relationships</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Relationships</em>' containment reference.
   * @see #setRelationships(Relationships)
   * @see fr.inria.lang.vM.VMPackage#getVariabilityModel_Relationships()
   * @model containment="true"
   * @generated
   */
  Relationships getRelationships();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.VariabilityModel#getRelationships <em>Relationships</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Relationships</em>' containment reference.
   * @see #getRelationships()
   * @generated
   */
  void setRelationships(Relationships value);

  /**
   * Returns the value of the '<em><b>Attributes</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Attributes</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Attributes</em>' containment reference.
   * @see #setAttributes(Attributes)
   * @see fr.inria.lang.vM.VMPackage#getVariabilityModel_Attributes()
   * @model containment="true"
   * @generated
   */
  Attributes getAttributes();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.VariabilityModel#getAttributes <em>Attributes</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Attributes</em>' containment reference.
   * @see #getAttributes()
   * @generated
   */
  void setAttributes(Attributes value);

  /**
   * Returns the value of the '<em><b>Descriptions</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Descriptions</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Descriptions</em>' containment reference.
   * @see #setDescriptions(Descriptions)
   * @see fr.inria.lang.vM.VMPackage#getVariabilityModel_Descriptions()
   * @model containment="true"
   * @generated
   */
  Descriptions getDescriptions();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.VariabilityModel#getDescriptions <em>Descriptions</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Descriptions</em>' containment reference.
   * @see #getDescriptions()
   * @generated
   */
  void setDescriptions(Descriptions value);

  /**
   * Returns the value of the '<em><b>Constraints</b></em>' containment reference.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Constraints</em>' containment reference isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Constraints</em>' containment reference.
   * @see #setConstraints(Constraints)
   * @see fr.inria.lang.vM.VMPackage#getVariabilityModel_Constraints()
   * @model containment="true"
   * @generated
   */
  Constraints getConstraints();

  /**
   * Sets the value of the '{@link fr.inria.lang.vM.VariabilityModel#getConstraints <em>Constraints</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @param value the new value of the '<em>Constraints</em>' containment reference.
   * @see #getConstraints()
   * @generated
   */
  void setConstraints(Constraints value);

} // VariabilityModel
