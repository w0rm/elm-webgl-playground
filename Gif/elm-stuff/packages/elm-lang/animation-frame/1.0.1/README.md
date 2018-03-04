# Getting Smooth Animations

Browsers have their own render loop, rerendering things as fast as possible. If you want smooth animations in your application, it is helpful to sync up with the browsers natural refresh rate.

This library helps you get messages in lockstep with the browser renders. This way you are not calculating too many or too few animation frames.

