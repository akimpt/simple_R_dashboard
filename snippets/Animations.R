ggplot(parking.turnover, aes(x = degrees)) +
  geom_histogram(colour = "black", bins = 96) + coord_polar() +
  transition_states(day, transition_length = 1, state_length = 1) +
  scale_x_continuous(breaks=seq(0,359,by=45),labels=c("12am", "3am", "6am", "9am","12pm", "3pm","6pm", "9pm")) +
  labs(title = 'Day: {closest_state}')

anim_save("PolarAnimationDays.gif", animation = last_animation(), width=10, height=10, dpi=600)