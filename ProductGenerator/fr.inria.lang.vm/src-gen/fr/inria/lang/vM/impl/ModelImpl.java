/**
 */
package fr.inria.lang.vM.impl;

import fr.inria.lang.vM.Configurations;
import fr.inria.lang.vM.ImportDeclaration;
import fr.inria.lang.vM.MetaData;
import fr.inria.lang.vM.Model;
import fr.inria.lang.vM.Objectives;
import fr.inria.lang.vM.PackageDeclaration;
import fr.inria.lang.vM.VMPackage;
import fr.inria.lang.vM.VariabilityModel;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Model</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getPackageDeclaration <em>Package Declaration</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getImportDeclarations <em>Import Declarations</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getMetainformation <em>Metainformation</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getVm <em>Vm</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getObjectives <em>Objectives</em>}</li>
 *   <li>{@link fr.inria.lang.vM.impl.ModelImpl#getConfigurations <em>Configurations</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ModelImpl extends MinimalEObjectImpl.Container implements Model
{
  /**
   * The cached value of the '{@link #getPackageDeclaration() <em>Package Declaration</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getPackageDeclaration()
   * @generated
   * @ordered
   */
  protected PackageDeclaration packageDeclaration;

  /**
   * The cached value of the '{@link #getImportDeclarations() <em>Import Declarations</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImportDeclarations()
   * @generated
   * @ordered
   */
  protected EList<ImportDeclaration> importDeclarations;

  /**
   * The cached value of the '{@link #getMetainformation() <em>Metainformation</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getMetainformation()
   * @generated
   * @ordered
   */
  protected MetaData metainformation;

  /**
   * The cached value of the '{@link #getVm() <em>Vm</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getVm()
   * @generated
   * @ordered
   */
  protected VariabilityModel vm;

  /**
   * The cached value of the '{@link #getObjectives() <em>Objectives</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getObjectives()
   * @generated
   * @ordered
   */
  protected Objectives objectives;

  /**
   * The cached value of the '{@link #getConfigurations() <em>Configurations</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getConfigurations()
   * @generated
   * @ordered
   */
  protected Configurations configurations;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected ModelImpl()
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
    return VMPackage.Literals.MODEL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public PackageDeclaration getPackageDeclaration()
  {
    return packageDeclaration;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetPackageDeclaration(PackageDeclaration newPackageDeclaration, NotificationChain msgs)
  {
    PackageDeclaration oldPackageDeclaration = packageDeclaration;
    packageDeclaration = newPackageDeclaration;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__PACKAGE_DECLARATION, oldPackageDeclaration, newPackageDeclaration);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setPackageDeclaration(PackageDeclaration newPackageDeclaration)
  {
    if (newPackageDeclaration != packageDeclaration)
    {
      NotificationChain msgs = null;
      if (packageDeclaration != null)
        msgs = ((InternalEObject)packageDeclaration).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__PACKAGE_DECLARATION, null, msgs);
      if (newPackageDeclaration != null)
        msgs = ((InternalEObject)newPackageDeclaration).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__PACKAGE_DECLARATION, null, msgs);
      msgs = basicSetPackageDeclaration(newPackageDeclaration, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__PACKAGE_DECLARATION, newPackageDeclaration, newPackageDeclaration));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<ImportDeclaration> getImportDeclarations()
  {
    if (importDeclarations == null)
    {
      importDeclarations = new EObjectContainmentEList<ImportDeclaration>(ImportDeclaration.class, this, VMPackage.MODEL__IMPORT_DECLARATIONS);
    }
    return importDeclarations;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public MetaData getMetainformation()
  {
    return metainformation;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetMetainformation(MetaData newMetainformation, NotificationChain msgs)
  {
    MetaData oldMetainformation = metainformation;
    metainformation = newMetainformation;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__METAINFORMATION, oldMetainformation, newMetainformation);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setMetainformation(MetaData newMetainformation)
  {
    if (newMetainformation != metainformation)
    {
      NotificationChain msgs = null;
      if (metainformation != null)
        msgs = ((InternalEObject)metainformation).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__METAINFORMATION, null, msgs);
      if (newMetainformation != null)
        msgs = ((InternalEObject)newMetainformation).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__METAINFORMATION, null, msgs);
      msgs = basicSetMetainformation(newMetainformation, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__METAINFORMATION, newMetainformation, newMetainformation));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public VariabilityModel getVm()
  {
    return vm;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetVm(VariabilityModel newVm, NotificationChain msgs)
  {
    VariabilityModel oldVm = vm;
    vm = newVm;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__VM, oldVm, newVm);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setVm(VariabilityModel newVm)
  {
    if (newVm != vm)
    {
      NotificationChain msgs = null;
      if (vm != null)
        msgs = ((InternalEObject)vm).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__VM, null, msgs);
      if (newVm != null)
        msgs = ((InternalEObject)newVm).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__VM, null, msgs);
      msgs = basicSetVm(newVm, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__VM, newVm, newVm));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Objectives getObjectives()
  {
    return objectives;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetObjectives(Objectives newObjectives, NotificationChain msgs)
  {
    Objectives oldObjectives = objectives;
    objectives = newObjectives;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__OBJECTIVES, oldObjectives, newObjectives);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setObjectives(Objectives newObjectives)
  {
    if (newObjectives != objectives)
    {
      NotificationChain msgs = null;
      if (objectives != null)
        msgs = ((InternalEObject)objectives).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__OBJECTIVES, null, msgs);
      if (newObjectives != null)
        msgs = ((InternalEObject)newObjectives).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__OBJECTIVES, null, msgs);
      msgs = basicSetObjectives(newObjectives, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__OBJECTIVES, newObjectives, newObjectives));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public Configurations getConfigurations()
  {
    return configurations;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetConfigurations(Configurations newConfigurations, NotificationChain msgs)
  {
    Configurations oldConfigurations = configurations;
    configurations = newConfigurations;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__CONFIGURATIONS, oldConfigurations, newConfigurations);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setConfigurations(Configurations newConfigurations)
  {
    if (newConfigurations != configurations)
    {
      NotificationChain msgs = null;
      if (configurations != null)
        msgs = ((InternalEObject)configurations).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__CONFIGURATIONS, null, msgs);
      if (newConfigurations != null)
        msgs = ((InternalEObject)newConfigurations).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - VMPackage.MODEL__CONFIGURATIONS, null, msgs);
      msgs = basicSetConfigurations(newConfigurations, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, VMPackage.MODEL__CONFIGURATIONS, newConfigurations, newConfigurations));
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
      case VMPackage.MODEL__PACKAGE_DECLARATION:
        return basicSetPackageDeclaration(null, msgs);
      case VMPackage.MODEL__IMPORT_DECLARATIONS:
        return ((InternalEList<?>)getImportDeclarations()).basicRemove(otherEnd, msgs);
      case VMPackage.MODEL__METAINFORMATION:
        return basicSetMetainformation(null, msgs);
      case VMPackage.MODEL__VM:
        return basicSetVm(null, msgs);
      case VMPackage.MODEL__OBJECTIVES:
        return basicSetObjectives(null, msgs);
      case VMPackage.MODEL__CONFIGURATIONS:
        return basicSetConfigurations(null, msgs);
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
      case VMPackage.MODEL__PACKAGE_DECLARATION:
        return getPackageDeclaration();
      case VMPackage.MODEL__IMPORT_DECLARATIONS:
        return getImportDeclarations();
      case VMPackage.MODEL__METAINFORMATION:
        return getMetainformation();
      case VMPackage.MODEL__VM:
        return getVm();
      case VMPackage.MODEL__OBJECTIVES:
        return getObjectives();
      case VMPackage.MODEL__CONFIGURATIONS:
        return getConfigurations();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case VMPackage.MODEL__PACKAGE_DECLARATION:
        setPackageDeclaration((PackageDeclaration)newValue);
        return;
      case VMPackage.MODEL__IMPORT_DECLARATIONS:
        getImportDeclarations().clear();
        getImportDeclarations().addAll((Collection<? extends ImportDeclaration>)newValue);
        return;
      case VMPackage.MODEL__METAINFORMATION:
        setMetainformation((MetaData)newValue);
        return;
      case VMPackage.MODEL__VM:
        setVm((VariabilityModel)newValue);
        return;
      case VMPackage.MODEL__OBJECTIVES:
        setObjectives((Objectives)newValue);
        return;
      case VMPackage.MODEL__CONFIGURATIONS:
        setConfigurations((Configurations)newValue);
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
      case VMPackage.MODEL__PACKAGE_DECLARATION:
        setPackageDeclaration((PackageDeclaration)null);
        return;
      case VMPackage.MODEL__IMPORT_DECLARATIONS:
        getImportDeclarations().clear();
        return;
      case VMPackage.MODEL__METAINFORMATION:
        setMetainformation((MetaData)null);
        return;
      case VMPackage.MODEL__VM:
        setVm((VariabilityModel)null);
        return;
      case VMPackage.MODEL__OBJECTIVES:
        setObjectives((Objectives)null);
        return;
      case VMPackage.MODEL__CONFIGURATIONS:
        setConfigurations((Configurations)null);
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
      case VMPackage.MODEL__PACKAGE_DECLARATION:
        return packageDeclaration != null;
      case VMPackage.MODEL__IMPORT_DECLARATIONS:
        return importDeclarations != null && !importDeclarations.isEmpty();
      case VMPackage.MODEL__METAINFORMATION:
        return metainformation != null;
      case VMPackage.MODEL__VM:
        return vm != null;
      case VMPackage.MODEL__OBJECTIVES:
        return objectives != null;
      case VMPackage.MODEL__CONFIGURATIONS:
        return configurations != null;
    }
    return super.eIsSet(featureID);
  }

} //ModelImpl
