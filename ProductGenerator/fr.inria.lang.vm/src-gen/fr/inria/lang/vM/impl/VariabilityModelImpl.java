/**
 */
package fr.inria.lang.vM.impl;

import fr.inria.lang.vM.Attributes;
import fr.inria.lang.vM.Constraints;
import fr.inria.lang.vM.Descriptions;
import fr.inria.lang.vM.Relationships;
import fr.inria.lang.vM.VMPackage;
import fr.inria.lang.vM.VariabilityModel;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Variability Model</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link fr.inria.lang.vM.impl.VariabilityModelImpl#getRelationships <em>Relationships</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.VariabilityModelImpl#getAttributes <em>Attributes</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.VariabilityModelImpl#getDescriptions <em>Descriptions</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.VariabilityModelImpl#getConstraints <em>Constraints</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class VariabilityModelImpl extends MinimalEObjectImpl.Container implements VariabilityModel
{
  /**
   * The cached value of the '{@link #getRelationships() <em>Relationships</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getRelationships()
   * @generated
   * @ordered
   */
  protected Relationships relationships;

  /**
   * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getAttributes()
   * @generated
   * @ordered
   */
  protected Attributes attributes;

  /**
   * The cached value of the '{@link #getDescriptions() <em>Descriptions</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getDescriptions()
   * @generated
   * @ordered
   */
  protected Descriptions descriptions;

  /**
   * The cached value of the '{@link #getConstraints() <em>Constraints</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getConstraints()
   * @generated
   * @ordered
   */
  protected Constraints constraints;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected VariabilityModelImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return VMPackage.Literals.VARIABILITY_MODEL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Relationships getRelationships()
  {
    return relationships;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetRelationships(Relationships newRelationships, NotificationChain msgs)
  {
    Relationships oldRelationships = relationships;
    relationships = newRelationships;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__RELATIONSHIPS, oldRelationships, newRelationships);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setRelationships(Relationships newRelationships)
  {
    if (newRelationships != relationships)
    {
      NotificationChain msgs = null;
      if (relationships != null)
        msgs = ((InternalEObject)relationships).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__RELATIONSHIPS, null, msgs);
      if (newRelationships != null)
        msgs = ((InternalEObject)newRelationships).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__RELATIONSHIPS, null, msgs);
      msgs = basicSetRelationships(newRelationships, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__RELATIONSHIPS, newRelationships, newRelationships));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Attributes getAttributes()
  {
    return attributes;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetAttributes(Attributes newAttributes, NotificationChain msgs)
  {
    Attributes oldAttributes = attributes;
    attributes = newAttributes;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__ATTRIBUTES, oldAttributes, newAttributes);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setAttributes(Attributes newAttributes)
  {
    if (newAttributes != attributes)
    {
      NotificationChain msgs = null;
      if (attributes != null)
        msgs = ((InternalEObject)attributes).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__ATTRIBUTES, null, msgs);
      if (newAttributes != null)
        msgs = ((InternalEObject)newAttributes).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__ATTRIBUTES, null, msgs);
      msgs = basicSetAttributes(newAttributes, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__ATTRIBUTES, newAttributes, newAttributes));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Descriptions getDescriptions()
  {
    return descriptions;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetDescriptions(Descriptions newDescriptions, NotificationChain msgs)
  {
    Descriptions oldDescriptions = descriptions;
    descriptions = newDescriptions;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__DESCRIPTIONS, oldDescriptions, newDescriptions);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setDescriptions(Descriptions newDescriptions)
  {
    if (newDescriptions != descriptions)
    {
      NotificationChain msgs = null;
      if (descriptions != null)
        msgs = ((InternalEObject)descriptions).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__DESCRIPTIONS, null, msgs);
      if (newDescriptions != null)
        msgs = ((InternalEObject)newDescriptions).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__DESCRIPTIONS, null, msgs);
      msgs = basicSetDescriptions(newDescriptions, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__DESCRIPTIONS, newDescriptions, newDescriptions));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Constraints getConstraints()
  {
    return constraints;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetConstraints(Constraints newConstraints, NotificationChain msgs)
  {
    Constraints oldConstraints = constraints;
    constraints = newConstraints;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__CONSTRAINTS, oldConstraints, newConstraints);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setConstraints(Constraints newConstraints)
  {
    if (newConstraints != constraints)
    {
      NotificationChain msgs = null;
      if (constraints != null)
        msgs = ((InternalEObject)constraints).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__CONSTRAINTS, null, msgs);
      if (newConstraints != null)
        msgs = ((InternalEObject)newConstraints).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.VARIABILITY_MODEL__CONSTRAINTS, null, msgs);
      msgs = basicSetConstraints(newConstraints, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.VARIABILITY_MODEL__CONSTRAINTS, newConstraints, newConstraints));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case VMPackage.VARIABILITY_MODEL__RELATIONSHIPS:
        return basicSetRelationships(null, msgs);
      case VMPackage.VARIABILITY_MODEL__ATTRIBUTES:
        return basicSetAttributes(null, msgs);
      case VMPackage.VARIABILITY_MODEL__DESCRIPTIONS:
        return basicSetDescriptions(null, msgs);
      case VMPackage.VARIABILITY_MODEL__CONSTRAINTS:
        return basicSetConstraints(null, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case VMPackage.VARIABILITY_MODEL__RELATIONSHIPS:
        return getRelationships();
      case VMPackage.VARIABILITY_MODEL__ATTRIBUTES:
        return getAttributes();
      case VMPackage.VARIABILITY_MODEL__DESCRIPTIONS:
        return getDescriptions();
      case VMPackage.VARIABILITY_MODEL__CONSTRAINTS:
        return getConstraints();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case VMPackage.VARIABILITY_MODEL__RELATIONSHIPS:
        setRelationships((Relationships)newValue);
        return;
      case VMPackage.VARIABILITY_MODEL__ATTRIBUTES:
        setAttributes((Attributes)newValue);
        return;
      case VMPackage.VARIABILITY_MODEL__DESCRIPTIONS:
        setDescriptions((Descriptions)newValue);
        return;
      case VMPackage.VARIABILITY_MODEL__CONSTRAINTS:
        setConstraints((Constraints)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case VMPackage.VARIABILITY_MODEL__RELATIONSHIPS:
        setRelationships((Relationships)null);
        return;
      case VMPackage.VARIABILITY_MODEL__ATTRIBUTES:
        setAttributes((Attributes)null);
        return;
      case VMPackage.VARIABILITY_MODEL__DESCRIPTIONS:
        setDescriptions((Descriptions)null);
        return;
      case VMPackage.VARIABILITY_MODEL__CONSTRAINTS:
        setConstraints((Constraints)null);
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case VMPackage.VARIABILITY_MODEL__RELATIONSHIPS:
        return relationships != null;
      case VMPackage.VARIABILITY_MODEL__ATTRIBUTES:
        return attributes != null;
      case VMPackage.VARIABILITY_MODEL__DESCRIPTIONS:
        return descriptions != null;
      case VMPackage.VARIABILITY_MODEL__CONSTRAINTS:
        return constraints != null;
    }
    return super.eIsSet(featureID);
  }

} //VariabilityModelImpl
