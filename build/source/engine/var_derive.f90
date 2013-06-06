module var_derive_module
USE nrtype
implicit none
private
public::calcHeight
public::rootDensty
public::satHydCond
public::fracFuture
public::v_shortcut
contains

 ! **********************************************************************************************************
 ! new subroutine: compute snow height
 ! **********************************************************************************************************
 subroutine calcHeight(err,message)
 USE data_struc,only:mpar_data,mvar_data,indx_data,ix_soil,ix_snow    ! data structures
 USE var_lookup,only:iLookPARAM,iLookMVAR,iLookINDEX                  ! named variables for structure elements
 implicit none
 ! declare dummy variables
 integer(i4b),intent(out) :: err               ! error code
 character(*),intent(out) :: message           ! error message
 ! declare pointers to data in model variable structures
 real(dp),pointer         :: mLayerDepth(:)    ! depth of the layer (m)
 real(dp),pointer         :: mLayerHeight(:)   ! height of the layer mid-point (m)
 real(dp),pointer         :: iLayerHeight(:)   ! height of the layer interface (m)
 ! declare pointers to model index variables
 integer(i4b),pointer     :: nLayers           ! number of layers
 integer(i4b),pointer     :: layerType(:)      ! type of the layer (ix_soil or ix_snow)
 ! declare local variables
 integer(i4b)             :: iLayer            ! loop through layers
 ! initialize error control
 err=0; message='calcHeight/'
 ! assign local pointers to the values in the model variable structures
 mLayerDepth    =>mvar_data%var(iLookMVAR%mLayerDepth)%dat             ! depth of the layer (m)
 mLayerHeight   =>mvar_data%var(iLookMVAR%mLayerHeight)%dat            ! height of the layer mid-point (m)
 iLayerHeight   =>mvar_data%var(iLookMVAR%iLayerHeight)%dat            ! height of the layer interface (m)
 ! assign local pointers to the model index structures
 nLayers        =>indx_data%var(iLookINDEX%nLayers)%dat(1)             ! number of layers
 layerType      =>indx_data%var(iLookINDEX%layerType)%dat              ! layer type (ix_soil or ix_snow)
 ! ************************************************************************************************************************
 ! initialize layer height as the top of the snowpack -- positive downward
 iLayerHeight(0) = -sum(mLayerDepth, mask=layerType==ix_snow)
 ! loop through layers
 do iLayer=1,nLayers
  ! compute the height at the layer midpoint
  mLayerHeight(iLayer) = iLayerHeight(iLayer-1) + mLayerDepth(iLayer)/2._dp
  ! compute the height at layer interfaces
  iLayerHeight(iLayer) = iLayerHeight(iLayer-1) + mLayerDepth(iLayer)
 end do ! (looping through layers)
 !print*, 'layerType   = ',  layerType
 !print*, 'mLayerDepth = ',  mLayerDepth
 !print*, 'mLayerHeight = ', mLayerHeight
 !print*, 'iLayerHeight = ', iLayerHeight
 !print*, '************** '
 end subroutine calcHeight


 ! **********************************************************************************************************
 ! new subroutine: compute vertical distribution of root density
 ! **********************************************************************************************************
 subroutine rootDensty(err,message)
 ! model decision structures
 USE data_struc,only:model_decisions        ! model decision structure
 USE var_lookup,only:iLookDECISIONS         ! named variables for elements of the decision structure
 ! look-up values for the choice of groundwater parameterization
 USE mDecisions_module,only: &
 bigBucket,                  & ! a big bucket (lumped aquifer model)
 noExplicit                    ! no explicit groundwater parameterization
 ! model variables, parameters, forcing data, etc.
 USE data_struc,only:mpar_data,mvar_data,indx_data,ix_soil,ix_snow    ! data structures
 USE var_lookup,only:iLookPARAM,iLookMVAR,iLookINDEX                  ! named variables for structure elements
 implicit none
 ! declare dummy variables
 integer(i4b),intent(out) :: err                   ! error code
 character(*),intent(out) :: message               ! error message
 ! declare pointers to data in parameter structures
 real(dp),pointer         :: rootingDepth          ! rooting depth (m)
 real(dp),pointer         :: rootDistExp           ! exponent for the vertical distriution of root density (-)
 ! declare pointers to data in model variable structures
 real(dp),pointer         :: scalarAquiferRootFrac ! fraction of roots below the soil profile (-)
 real(dp),pointer         :: mLayerRootDensity(:)  ! fraction of roots in each soil layer (-)
 real(dp),pointer         :: iLayerHeight(:)       ! height of the layer interface (m)
 ! declare pointers to model index variables
 integer(i4b),pointer     :: nLayers               ! number of layers
 integer(i4b),pointer     :: layerType(:)          ! type of the layer (ix_soil or ix_snow)
 ! declare local variables
 integer(i4b)             :: nSoil,nSnow           ! number of soil and snow layers
 integer(i4b)             :: iLayer                ! loop through layers
 real(dp)                 :: fracRootLower         ! fraction of the rooting depth at the lower interface
 real(dp)                 :: fracRootUpper         ! fraction of the rooting depth at the upper interface
 real(dp)                 :: checkCalcs            ! check calculations for aquifer roots
 ! initialize error control
 err=0; message='rootDensty/'

 ! assign local pointers to the values in the parameter structures
 rootingDepth          =>mpar_data%var(iLookPARAM%rootingDepth)                ! rooting depth (m)
 rootDistExp           =>mpar_data%var(iLookPARAM%rootDistExp)                 ! root distribution exponent (-)
 ! assign local pointers to the values in the model variable structures
 scalarAquiferRootFrac =>mvar_data%var(iLookMVAR%scalarAquiferRootFrac)%dat(1) ! fraction of roots below the soil profile (in the aquifer)
 mLayerRootDensity     =>mvar_data%var(iLookMVAR%mLayerRootDensity)%dat        ! fraction of roots in each soil layer (-)
 iLayerHeight          =>mvar_data%var(iLookMVAR%iLayerHeight)%dat             ! height of the layer interface (m)
 ! assign local pointers to the model index structures
 nLayers               =>indx_data%var(iLookINDEX%nLayers)%dat(1)              ! number of layers
 layerType             =>indx_data%var(iLookINDEX%layerType)%dat               ! layer type (ix_soil or ix_snow)
 ! identify the number of snow and soil layers
 nSnow = count(layerType==ix_snow)
 nSoil = count(layerType==ix_soil)

 ! check that the rooting depth is less than the soil depth
 if(model_decisions(iLookDECISIONS%groundwatr)%iDecision /= bigBucket)then
  if(rootingDepth>iLayerHeight(nLayers))then; err=10; message=trim(message)//'rooting depth can ONLY exceed soil depth for the big bucket gw parameterization'; return; endif
 endif

 ! compute the fraction of roots in each soil layer
 do iLayer=nSnow+1,nLayers
  if(iLayerHeight(iLayer-1)<rootingDepth)then
   ! compute the fraction of the rooting depth at the lower and upper interfaces
   fracRootLower = iLayerHeight(iLayer-1)/rootingDepth
   fracRootUpper = iLayerHeight(iLayer)/rootingDepth
   if(fracRootUpper>1._dp) fracRootUpper=1._dp
   ! compute the root density
   mLayerRootDensity(iLayer-nSnow) = fracRootUpper**rootDistExp - fracRootLower**rootDistExp
  else
   mLayerRootDensity(iLayer-nSnow) = 0._dp
  endif
  !print*, 'iLayerHeight(iLayer-1:iLayer) = ', iLayerHeight(iLayer-1:iLayer)
  !write(*,'(a,10(f11.5,1x))') 'mLayerRootDensity(iLayer-nSnow), fracRootUpper, fracRootLower, fracRootUpper**rootDistExp, fracRootLower**rootDistExp = ', &
  !                             mLayerRootDensity(iLayer-nSnow), fracRootUpper, fracRootLower, fracRootUpper**rootDistExp, fracRootLower**rootDistExp
 end do  ! (looping thru layers)
 !pause

 ! compute fraction of roots in the aquifer
 if(rootingDepth > iLayerHeight(nLayers))then
  scalarAquiferRootFrac = 1._dp - sum(mLayerRootDensity(1:nSoil))
  checkCalcs = 1._dp - ( min(iLayerHeight(nLayers),rootingDepth) / rootingDepth)**rootDistExp
  if(abs(checkCalcs - scalarAquiferRootFrac) > epsilon(checkCalcs))then
   err=20; message=trim(message)//'problem with the aquifer root density calculations'; return
  endif
 
 ! set fraction of aquifer roots to zero, and check everything is OK
 else
  scalarAquiferRootFrac = 0._dp
  if(abs(sum(mLayerRootDensity) - 1._dp) > epsilon(rootingDepth))then
   message=trim(message)//'root density does not sum to one when rooting depth is within the soil profile'
   err=20; return
  endif
 endif

 !print*, 'iLayerHeight(nLayers), rootingDepth, scalarAquiferRootFrac = ', iLayerHeight(nLayers), rootingDepth, scalarAquiferRootFrac
 !pause

 end subroutine rootDensty

 ! **********************************************************************************************************
 ! new subroutine: compute vertical profile of saturated hydraulic conductivity
 ! **********************************************************************************************************
 subroutine satHydCond(err,message)
 ! model decision structures
 USE data_struc,only:model_decisions        ! model decision structure
 USE var_lookup,only:iLookDECISIONS         ! named variables for elements of the decision structure
 ! look-up values for the choice of groundwater parameterization
 USE mDecisions_module,only: &
  constant,                  & ! constant hydraulic conductivity with depth
  powerLaw_profile             ! power-law profile
 ! model variables, parameters, forcing data, etc.
 USE data_struc,only:mpar_data,mvar_data,indx_data,ix_soil,ix_snow    ! data structures
 USE var_lookup,only:iLookPARAM,iLookMVAR,iLookINDEX                  ! named variables for structure elements
 implicit none
 ! declare dummy variables
 integer(i4b),intent(out) :: err                   ! error code
 character(*),intent(out) :: message               ! error message
 ! declare pointers to data in parameter structures
 real(dp),pointer         :: k_soil                ! saturated hydraulic conductivity at the compacted depth (m s-1)
 real(dp),pointer         :: compactedDepth        ! the depth at which k_soil reaches the compacted value given by CH78 (m)
 real(dp),pointer         :: aquiferBaseflowExp    ! exponent for the TOPMODEL-ish baseflow parameterization (-)
 ! declare pointers to data in model variable structures
 real(dp),pointer         :: mLayerSatHydCond(:)   ! saturated hydraulic conductivity at the mid-point of each layer (m s-1)
 real(dp),pointer         :: iLayerSatHydCond(:)   ! saturated hydraulic conductivity at the interface of each layer (m s-1)
 real(dp),pointer         :: mLayerHeight(:)       ! height at the mid-point of each layer (m)
 real(dp),pointer         :: iLayerHeight(:)       ! height at the interface of each layer (m)
 ! declare pointers to model index variables
 integer(i4b),pointer     :: nLayers               ! number of layers
 integer(i4b),pointer     :: layerType(:)          ! type of the layer (ix_soil or ix_snow)
 ! declare local variables
 integer(i4b)             :: nSoil,nSnow           ! number of soil and snow layers
 integer(i4b)             :: iLayer                ! loop through layers
 ! initialize error control
 err=0; message='satHydCond/'

 ! assign local pointers to the values in the parameter structures
 k_soil             => mpar_data%var(iLookPARAM%k_soil)              ! saturated hydraulic conductivity at the compacted depth (m s-1)
 compactedDepth     => mpar_data%var(iLookPARAM%compactedDepth)      ! the depth at which k_soil reaches the compacted value given by CH78 (m)
 aquiferBaseflowExp => mpar_data%var(iLookPARAM%aquiferBaseflowExp)  ! exponent for the TOPMODEL-ish baseflow parameterization (-)
 ! assign local pointers to the values in the model variable structures
 mLayerSatHydCond   => mvar_data%var(iLookMVAR%mLayerSatHydCond)%dat ! saturated hydraulic conductivity at the mid-point of each layer (m s-1)
 iLayerSatHydCond   => mvar_data%var(iLookMVAR%iLayerSatHydCond)%dat ! saturated hydraulic conductivity at the interface of each layer (m s-1)
 mLayerHeight       => mvar_data%var(iLookMVAR%mLayerHeight)%dat     ! height at the mid-point of each layer (m)
 iLayerHeight       => mvar_data%var(iLookMVAR%iLayerHeight)%dat     ! height at the interface of each layer (m)
 ! assign local pointers to the model index structures
 nLayers            => indx_data%var(iLookINDEX%nLayers)%dat(1)      ! number of layers
 layerType          => indx_data%var(iLookINDEX%layerType)%dat       ! layer type (ix_soil or ix_snow)
 ! identify the number of snow and soil layers
 nSnow = count(layerType==ix_snow)
 nSoil = count(layerType==ix_soil)

 ! loop through soil layers
 ! NOTE: could do constant profile with the power-law profile with exponent=1, but keep constant profile decision for clarity
 do iLayer=nSnow,nLayers
  select case(model_decisions(iLookDECISIONS%hc_profile)%iDecision)
   ! constant hydraulic conductivity with depth
   case(constant)
    iLayerSatHydCond(iLayer-nSnow) = k_soil
    if(iLayer > nSnow)& ! avoid layer 0
     mLayerSatHydCond(iLayer-nSnow) = k_soil
   ! power-law profile
   case(powerLaw_profile)
    iLayerSatHydCond(iLayer-nSnow) = k_soil * ( (1._dp - iLayerHeight(iLayer)/iLayerHeight(nLayers))**(aquiferBaseflowExp - 1._dp) ) &
                                            / ( (1._dp -       compactedDepth/iLayerHeight(nLayers))**(aquiferBaseflowExp - 1._dp) )
    if(iLayer > nSnow)& ! avoid layer 0
     mLayerSatHydCond(iLayer-nSnow) = k_soil * ( (1._dp - mLayerHeight(iLayer)/iLayerHeight(nLayers))**(aquiferBaseflowExp - 1._dp) ) &
                                             / ( (1._dp -       compactedDepth/iLayerHeight(nLayers))**(aquiferBaseflowExp - 1._dp) )
   ! error check (errors checked earlier also, so should not get here)
   case default
    message=trim(message)//"unknown hydraulic conductivity profile [option="//trim(model_decisions(iLookDECISIONS%hc_profile)%cDecision)//"]"
    err=10; return
  end select
  !if(iLayer > nSnow)& ! avoid layer 0
  ! write(*,'(i4,1x,2(f11.5,1x,e20.10,1x))') iLayer, mLayerHeight(iLayer), mLayerSatHydCond(iLayer-nSnow), iLayerHeight(iLayer), iLayerSatHydCond(iLayer-nSnow)
 end do  ! looping through soil layers
 !print*, trim(model_decisions(iLookDECISIONS%hc_profile)%cDecision)
 !pause ' in satHydCond'

 end subroutine satHydCond




 ! **********************************************************************************************************
 ! new subroutine: compute the fraction of runoff in future time steps
 ! **********************************************************************************************************
 subroutine fracFuture(err,message)
 ! model decision structures
 USE data_struc,only:model_decisions                  ! model decision structure
 USE var_lookup,only:iLookDECISIONS                   ! named variables for elements of the decision structure
 ! look-up values for the sub-grid routing method
 USE mDecisions_module,only:      &
  timeDelay,&  ! time-delay histogram
  qInstant     ! instantaneous routing
 ! model variables, parameters, forcing data, etc.
 USE data_struc,only:data_step                        ! time step of forcing data
 USE data_struc,only:bvar_data,bpar_data              ! data structures for model variables and parameters
 USE var_lookup,only:iLookBVAR,iLookBPAR              ! named variables for structure elements
 implicit none
 ! dummy variables
 integer(i4b),intent(out)   :: err                    ! error code
 character(*),intent(out)   :: message                ! error message
 ! pointers to model structures
 real(dp)                   :: dt                     ! data time step (s)
 integer(i4b),pointer       :: ixRouting              ! index for routing method
 real(dp),pointer           :: routingGammaShape      ! shape parameter in Gamma distribution used for sub-grid routing (-)
 real(dp),pointer           :: routingGammaScale      ! scale parameter in Gamma distribution used for sub-grid routing (s)
 real(dp),pointer           :: runoffFuture(:)        ! runoff in future time steps (m s-1)
 real(dp),pointer           :: fractionFuture(:)      ! fraction of runoff in future time steps (-)
 ! internal
 integer(i4b)               :: nTDH                   ! number of points in the time-delay histogram 
 integer(i4b)               :: iFuture                ! index in time delay histogram
 real(dp)                   :: aLambda                ! scale parameter in the Gamma distribution
 real(dp)                   :: tFuture                ! future time (end of step)
 real(dp)                   :: pSave                  ! cumulative probability at the start of the step
 real(dp)                   :: cumProb                ! cumulative probability at the end of the step
 real(dp)                   :: sumFrac                ! sum of runoff fractions in all steps
 real(dp),parameter         :: tolerFrac=0.01_dp      ! tolerance for fractional runoff
 ! initialize error control
 err=0; message='fracFuture/'

 ! assign pointers (just to save typing)
 dt                =  data_step                                              ! get the legth of the data step (s)
 ixRouting         => model_decisions(iLookDECISIONS%subRouting)%iDecision   ! index for routing method
 routingGammaShape => bpar_data%var(iLookBPAR%routingGammaShape)             ! shape parameter in Gamma distribution used for sub-grid routing (-)
 routingGammaScale => bpar_data%var(iLookBPAR%routingGammaScale)             ! scale parameter in Gamma distribution used for sub-grid routing (s)
 runoffFuture      => bvar_data%var(iLookBVAR%routingRunoffFuture)%dat       ! runoff in future time steps (m s-1)
 fractionFuture    => bvar_data%var(iLookBVAR%routingFractionFuture)%dat     ! fraction of runoff in future time steps (-)

 ! identify number of points in the time-delay histogram
 nTDH = size(runoffFuture)

 ! initialize runoffFuture
 runoffFuture(1:nTDH) = 0._dp

 print*, 'nTDH = ', nTDH

 ! select option for sub-grid routing 
 select case(ixRouting)

  ! ** instantaneous routing
  case(qInstant)
   fractionFuture(1)      = 1._dp
   fractionFuture(2:nTDH) = 0._dp

  ! ** time delay histogram
  case(timeDelay)
   ! initialize
   pSave   = 0._dp ! cumulative probability at the start of the step
   aLambda = routingGammaShape / routingGammaScale
   if(routingGammaShape <= 0._dp .or. aLambda < 0._dp)then
    message=trim(message)//'bad arguments for the Gamma distribution'
    err=20; return
   endif
   ! loop through time steps and compute fraction of runoff in future steps
   do iFuture = 1,nTDH
    tFuture = real(iFuture, kind(dt))*dt                  ! future time (end of step)
    cumProb = gammp(routingGammaShape,aLambda*tFuture)    ! cumulative probability at the end of the step
    fractionFuture(iFuture) = max(0._dp, cumProb - pSave) ! fraction of runoff in the current step
    pSave   = cumProb                                     ! save the cumulative probability for use in the next step
    if(fractionFuture(iFuture) < tiny(dt))then
     fractionFuture(iFuture:nTDH) = 0._dp
     exit
    endif
    !write(*,'(a,1x,i4,1x,3(f20.10,1x))') trim(message), iFuture, tFuture, cumProb, fractionFuture(iFuture)
   end do ! (looping through future time steps)
   ! check that we have enough bins
   sumFrac  = sum(fractionFuture)
   if(abs(1._dp - sumFrac) > tolerFrac)then
    message=trim(message)//'not enough bins for the time delay histogram -- fix hard-coded parameter in XXX'
    err=20; return
   endif
   ! ensure the fraction sums to one
   fractionFuture = fractionFuture/sumFrac

  ! ** error checking
  case default; err=20; message=trim(message)//'cannot find option for sub-grid routing'; return

 end select ! (select option for sub-grid routing)

 end subroutine fracFuture




 ! **********************************************************************************************************
 ! new subroutine: compute "short-cut" variables
 ! **********************************************************************************************************
 subroutine v_shortcut(err,message)
 ! used to compute derived model variables
 USE multiconst, only:&
                       LH_fus,    &            ! latent heat of fusion                (J kg-1)
                       Cp_air,    &            ! specific heat of air                 (J kg-1 K-1)
                       Cp_ice,    &            ! specific heat of ice                 (J kg-1 K-1)
                       Cp_soil,   &            ! specific heat of soil                (J kg-1 K-1)
                       Cp_water,  &            ! specific heat of liquid water        (J kg-1 K-1)
                       iden_air,  &            ! intrinsic density of air             (kg m-3)
                       iden_ice,  &            ! intrinsic density of ice             (kg m-3)
                       iden_water,&            ! intrinsic density of liquid water    (kg m-3)
                       gravity,   &            ! gravitational acceleration           (m s-2)
                       Tfreeze                 ! freezing point of pure water         (K)
 USE data_struc,only:mpar_data,mvar_data,indx_data,ix_soil,ix_snow    ! data structures
 USE var_lookup,only:iLookPARAM,iLookMVAR,iLookINDEX                  ! named variables for structure elements
 implicit none
 ! declare dummy variables
 integer(i4b),intent(out) :: err               ! error code
 character(*),intent(out) :: message           ! error message
 ! declare pointers to data in parameter structures
 real(dp),pointer         :: iden_soil         ! intrinsic density of soil (kg m-3)
 real(dp),pointer         :: frac_sand         ! fraction of sand (-)
 real(dp),pointer         :: frac_silt         ! fraction of silt (-)
 real(dp),pointer         :: frac_clay         ! fraction of clay (-)
 real(dp),pointer         :: theta_sat         ! soil porosity (-)
 real(dp),pointer         :: vGn_n             ! van Genutchen "n" parameter (-)
 real(dp),pointer         :: kappa             ! constant in the freezing curve function (m K-1)
 ! declare pointers to data in model variable structures
 real(dp),pointer         :: vGn_m             ! van Genutchen "m" parameter (-)
 real(dp),pointer         :: volHtCap_air      ! volumetric heat capacity of air (J m-3 K-1)
 real(dp),pointer         :: volHtCap_ice      ! volumetric heat capacity of ice (J m-3 K-1)
 real(dp),pointer         :: volHtCap_soil     ! volumetric heat capacity of soil (J m-3 K-1)
 real(dp),pointer         :: volHtCap_water    ! volumetric heat capacity of water (J m-3 K-1)
 real(dp),pointer         :: lambda_drysoil    ! thermal conductivity of dry soil (W m-1)
 real(dp),pointer         :: lambda_wetsoil    ! thermal conductivity of wet soil (W m-1)
 real(dp),pointer         :: volLatHt_fus      ! volumetric latent heat of fusion (J m-3)
 ! declare local variables
 real(dp)                 :: bulkden_soil      ! bulk density of soil (kg m-3)
 ! initialize error control
 err=0; message='v_shortcut/'
 ! assign local pointers to the values in the parameter structures
 iden_soil      =>mpar_data%var(iLookPARAM%soil_dens_intr)             ! intrinsic soil density (kg m-3)
 frac_sand      =>mpar_data%var(iLookPARAM%frac_sand)                  ! fraction of sand (-)
 frac_silt      =>mpar_data%var(iLookPARAM%frac_silt)                  ! fraction of silt (-)
 frac_clay      =>mpar_data%var(iLookPARAM%frac_clay)                  ! fraction of clay (-)
 theta_sat      =>mpar_data%var(iLookPARAM%theta_sat)                  ! soil porosity (-)
 vGn_n          =>mpar_data%var(iLookPARAM%vGn_n)                      ! van Genutchen "n" parameter (-)
 ! assign local pointers to the values in the model variable structures
 vGn_m          =>mvar_data%var(iLookMVAR%scalarVGn_m)%dat(1)          ! van Genutchen "m" parameter (-)
 kappa          =>mvar_data%var(iLookMVAR%scalarKappa)%dat(1)          ! constant in the freezing curve function (m K-1)
 volHtCap_air   =>mvar_data%var(iLookMVAR%scalarVolHtCap_air)%dat(1)   ! volumetric heat capacity of air (J m-3 K-1)
 volHtCap_ice   =>mvar_data%var(iLookMVAR%scalarVolHtCap_ice)%dat(1)   ! volumetric heat capacity of ice (J m-3 K-1)
 volHtCap_soil  =>mvar_data%var(iLookMVAR%scalarVolHtCap_soil)%dat(1)  ! volumetric heat capacity of soil (J m-3 K-1)
 volHtCap_water =>mvar_data%var(iLookMVAR%scalarVolHtCap_water)%dat(1) ! volumetric heat capacity of water (J m-3 K-1)
 lambda_drysoil =>mvar_data%var(iLookMVAR%scalarLambda_drysoil)%dat(1) ! thermal conductivity of dry soil (W m-1)
 lambda_wetsoil =>mvar_data%var(iLookMVAR%scalarLambda_wetsoil)%dat(1) ! thermal conductivity of wet soil (W m-1)
 volLatHt_fus   =>mvar_data%var(iLookMVAR%scalarvolLatHt_fus)%dat(1)   ! volumetric latent heat of fusion (J m-3)
 ! ************************************************************************************************************************
 ! compute the van Genutchen "m" parameter
 vGn_m = 1._dp - 1._dp/vGn_n
 ! ************************************************************************************************************************
 ! compute the constant in the freezing curve function (m K-1)
 kappa  = (iden_ice/iden_water)*(LH_fus/(gravity*Tfreeze))  ! NOTE: J = kg m2 s-2
 ! ************************************************************************************************************************
 ! compute volumetric heat capacity (J m-3 K-1)
 volHtCap_air   = iden_air   * Cp_air
 volHtCap_ice   = iden_ice   * Cp_Ice
 volHtCap_soil  = iden_soil  * Cp_soil
 volHtCap_water = iden_water * Cp_water
 ! compute the thermal conductivity of dry and wet soils (W m-1)
 bulkden_soil   = iden_soil*(1._dp - theta_sat)
 lambda_drysoil = (0.135_dp*bulkden_soil + 64.7_dp) / (iden_soil - 0.947_dp*bulkden_soil)
 lambda_wetsoil = (8.80_dp*frac_sand + 2.92_dp*frac_clay) / (frac_sand + frac_clay)
 !print*, 'frac_sand, frac_silt, frac_clay = ', frac_sand, frac_silt, frac_clay
 !print*, 'lambda_drysoil, lambda_wetsoil = ', lambda_drysoil, lambda_wetsoil
 !print*, 'volHtCap_soil = ', volHtCap_soil
 ! compute the volumetric latent heat of fusion (J m-3)
 volLatHt_fus = iden_ice   * LH_fus
 ! ************************************************************************************************************************
 end subroutine v_shortcut




 ! ************************************************************************************************
 ! private numerical recipes function: incomplete Gamma function 
 ! ************************************************************************************************
 FUNCTION gammp(a,x)
 IMPLICIT NONE
 REAL(DP), INTENT(IN) :: a,x
 REAL(DP) :: gammp
 if (x<a+1.0_dp) then
  gammp=gser(a,x)
 else
  gammp=1.0_dp-gcf(a,x)
 end if
 END FUNCTION gammp

 FUNCTION gcf(a,x,gln)
 IMPLICIT NONE
 REAL(DP), INTENT(IN) :: a,x
 REAL(DP), OPTIONAL, INTENT(OUT) :: gln
 REAL(DP) :: gcf
 INTEGER(I4B), PARAMETER :: ITMAX=100
 REAL(DP), PARAMETER :: EPS=epsilon(x),FPMIN=tiny(x)/EPS
 INTEGER(I4B) :: i
 REAL(DP) :: an,b,c,d,del,h
 if (x == 0.0) then
  gcf=1.0
  RETURN
 end if
 b=x+1.0_dp-a
 c=1.0_dp/FPMIN
 d=1.0_dp/b
 h=d
 do i=1,ITMAX
  an=-i*(i-a)
  b=b+2.0_dp
  d=an*d+b
  if (abs(d) < FPMIN) d=FPMIN
  c=b+an/c
  if (abs(c) < FPMIN) c=FPMIN
  d=1.0_dp/d
  del=d*c
  h=h*del
  if (abs(del-1.0_dp) <= EPS) exit
 end do
 if (i > ITMAX) stop 'a too large, ITMAX too small in gcf'
 if (present(gln)) then
  gln=gammln(a)
  gcf=exp(-x+a*log(x)-gln)*h
 else
  gcf=exp(-x+a*log(x)-gammln(a))*h
 end if
 END FUNCTION gcf

 FUNCTION gser(a,x,gln)
 IMPLICIT NONE
 REAL(DP), INTENT(IN) :: a,x
 REAL(DP), OPTIONAL, INTENT(OUT) :: gln
 REAL(DP) :: gser
 INTEGER(I4B), PARAMETER :: ITMAX=100
 REAL(DP), PARAMETER :: EPS=epsilon(x)
 INTEGER(I4B) :: n
 REAL(DP) :: ap,del,summ
 if (x == 0.0) then
  gser=0.0
  RETURN
 end if
 ap=a
 summ=1.0_dp/a
 del=summ
 do n=1,ITMAX
  ap=ap+1.0_dp
  del=del*x/ap
  summ=summ+del
  if (abs(del) < abs(summ)*EPS) exit
 end do
 if (n > ITMAX) stop 'a too large, ITMAX too small in gser'
 if (present(gln)) then
  gln=gammln(a)
  gser=summ*exp(-x+a*log(x)-gln)
 else
  gser=summ*exp(-x+a*log(x)-gammln(a))
 end if
 END FUNCTION gser

 FUNCTION gammln(xx)
 USE nr_utility_module,only:arth  ! use to build vectors with regular increments
 IMPLICIT NONE
 REAL(DP), INTENT(IN) :: xx
 REAL(DP) :: gammln
 REAL(DP) :: tmp,x
 REAL(DP) :: stp = 2.5066282746310005_dp
 REAL(DP), DIMENSION(6) :: coef = (/76.18009172947146_dp,&
  -86.50532032941677_dp,24.01409824083091_dp,&
  -1.231739572450155_dp,0.1208650973866179e-2_dp,&
  -0.5395239384953e-5_dp/)
 if(xx <= 0._dp) stop 'xx > 0 in gammln'
 x=xx
 tmp=x+5.5_dp
 tmp=(x+0.5_dp)*log(tmp)-tmp
 gammln=tmp+log(stp*(1.000000000190015_dp+&
  sum(coef(:)/arth(x+1.0_dp,1.0_dp,size(coef))))/x)
 END FUNCTION gammln





end module var_derive_module
