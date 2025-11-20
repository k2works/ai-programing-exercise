import { User } from '../../domain/auth/User';
import { IUserRepository } from '../../domain/auth/IUserRepository';

export class UserManagementService {
  constructor(private readonly userRepository: IUserRepository) {}

  async registerUser(
    id: string,
    firstName: string,
    lastName: string,
    password: string,
    roleName: string,
    userType: string,
    createdBy: string
  ): Promise<User> {
    const user = await User.createWithHashedPassword(
      id,
      firstName,
      lastName,
      password,
      roleName,
      userType,
      createdBy
    );

    await this.userRepository.save(user);
    return user;
  }

  async updateUser(
    id: string,
    firstName?: string,
    lastName?: string
  ): Promise<User> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }

    // Note: User entity would need update methods for firstName/lastName
    // For now, we'll create a new instance with updated values
    const updatedUser = User.reconstruct(
      user.getId(),
      firstName ?? user.getFirstName(),
      lastName ?? user.getLastName(),
      user.toJSON().password,
      user.getRoleName(),
      user.getUserType(),
      user.getStatus(),
      user.toJSON().createdBy,
      user.toJSON().createdAt,
      new Date()
    );

    await this.userRepository.save(updatedUser);
    return updatedUser;
  }

  async deactivateUser(id: string): Promise<void> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }

    user.deactivate();
    await this.userRepository.save(user);
  }

  async reactivateUser(id: string): Promise<void> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }

    user.activate();
    await this.userRepository.save(user);
  }

  async deleteUser(id: string): Promise<void> {
    const user = await this.userRepository.findById(id);
    if (!user) {
      throw new Error('User not found');
    }

    await this.userRepository.delete(id);
  }
}
